﻿module FSharpLint.Console.Program

open Argu
open System
open System.IO
open System.Text
open FSharpLint.Framework
open FSharpLint.Application


/// Output format the linter will use.
type private OutputFormat =
    | Standard = 1
    | MSBuild = 2

/// File type the linter is running against.
type private FileType =
    | Project = 1
    | Solution = 2
    | File = 3
    | Source = 4

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of OutputFormat
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
    | [<CliPrefix(CliPrefix.None)>] Fix of ParseResults<FixArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."
            | Fix _ -> "Apply quickfixes for specified rule name or names (comma separated)."

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private LintArgs =
    | [<MainCommand; Mandatory>] Target of target:string
    | [<AltCommandLine("-l")>] Lint_Config of lintConfig:string
    | File_Type of FileType
// fsharplint:enable UnionDefinitionIndentation
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "Input to lint."
            | File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Lint_Config _ -> "Path to the config for the lint."
// fsharplint:enable UnionCasesNames

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private FixArgs =
    | [<MainCommand; Mandatory>] Fix_Target of ruleName:string * target:string
    | [<AltCommandLine("-l")>] Fix_Config of lintConfig:string
    | Fix_File_Type of FileType
// fsharplint:enable UnionDefinitionIndentation
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Fix_Target _ -> "Rule name to be applied with suggestedFix and input to lint."
            | Fix_File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Fix_Config _ -> "Path to the config for the lint."
// fsharplint:enable UnionCasesNames

let private parserProgress (output:Output.IOutput) = function
    | Starting file ->
        String.Format(Resources.GetString("ConsoleStartingFile"), file) |> output.WriteInfo
    | ReachedEnd (_, warnings) ->
        String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> output.WriteInfo
    | Failed (file, parseException) ->
        String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> output.WriteError
        "Exception Message:" + Environment.NewLine +
            parseException.Message + Environment.NewLine +
            "Exception Stack Trace:" + Environment.NewLine +
            parseException.StackTrace + Environment.NewLine
        |> output.WriteError

/// Infers the file type of the target based on its file extension.
let private inferFileType (target:string) =
    if target.EndsWith ".fs" || target.EndsWith ".fsx" then
        FileType.File
    else if target.EndsWith ".fsproj" then
        FileType.Project
    else if target.EndsWith ".sln" then
        FileType.Solution
    else
        FileType.Source

let private start (arguments:ParseResults<ToolArgs>) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
    let mutable exitCode = 0

    let output =
        match arguments.TryGetResult Format with
        | Some OutputFormat.MSBuild -> Output.MSBuildOutput() :> Output.IOutput
        | Some OutputFormat.Standard
        | Some _
        | None -> Output.StandardOutput() :> Output.IOutput

    let handleError (status:int) (str:string) =
        output.WriteError str
        exitCode <- status

    let outputWarnings (warnings:list<Suggestion.LintWarning>) =
        String.Format(Resources.GetString("ConsoleFinished"), List.length warnings)
            |> output.WriteInfo
    
    let handleLintResult = function
        | LintResult.Success(warnings) ->
            outputWarnings warnings
            if List.isEmpty warnings |> not then 
               exitCode <- -1
        | LintResult.Failure failure -> handleError -1 failure.Description
    
    let handleFixResult (ruleName: string) = function
        | LintResult.Success(warnings) ->
            Resources.GetString "ConsoleApplyingSuggestedFixFile" |> output.WriteInfo
            let mutable countSuggestedFix = 0
            List.iter (fun (element: Suggestion.LintWarning) ->
                let sourceCode = File.ReadAllText element.FilePath
                if String.Equals(ruleName, element.RuleName, StringComparison.InvariantCultureIgnoreCase) then
                    match element.Details.SuggestedFix with
                    | Some suggestedFix ->
                        suggestedFix.Force()
                        |> Option.map (fun suggestedFix ->
                            let updatedSourceCode = sourceCode.Replace(suggestedFix.FromText, suggestedFix.ToText)
                            File.WriteAllText(element.FilePath, updatedSourceCode, Encoding.UTF8)) 
                            |> ignore |> fun () -> countSuggestedFix <-countSuggestedFix + 1
                    | None -> ()
                else
                    ()) warnings
            outputWarnings warnings
            if countSuggestedFix = 0 then
                exitCode<-2

        | LintResult.Failure failure -> handleError -1 failure.Description

    let linting fileType lintParams target toolsPath shouldFix maybeRuleName =
        try
            let lintResult =
                match fileType with
                | FileType.File -> Lint.lintFile lintParams target
                | FileType.Source -> Lint.lintSource lintParams target
                | FileType.Solution -> Lint.lintSolution lintParams target toolsPath
                | FileType.Project
                | _ -> Lint.lintProject lintParams target toolsPath
            if shouldFix then
                match maybeRuleName with
                | Some ruleName -> handleFixResult ruleName lintResult
                | None -> exitCode <- 1
            else
                handleLintResult lintResult
        with
        | e ->
            let target = if fileType = FileType.Source then "source" else target
            sprintf "Lint failed while analysing %s.\nFailed with: %s\nStack trace: %s" target e.Message e.StackTrace
            |> (handleError -1)

    let getParams config =
        let paramConfig =
            match config with
            | Some configPath -> FromFile configPath
            | None -> Default

        { CancellationToken = None
          ReceivedWarning = Some output.WriteWarning
          Configuration = paramConfig
          ReportLinterProgress = parserProgress output |> Some }

    let applyLint (lintArgs: ParseResults<LintArgs>) =
        let lintConfig = lintArgs.TryGetResult Lint_Config

        let lintParams = getParams lintConfig
        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        linting fileType lintParams target toolsPath false None

    let applySuggestedFix (fixArgs: ParseResults<FixArgs>) =
        let fixConfig = fixArgs.TryGetResult Fix_Config

        let fixParams = getParams fixConfig
        let ruleName, target = fixArgs.GetResult Fix_Target
        let fileType = fixArgs.TryGetResult Fix_File_Type |> Option.defaultValue (inferFileType target)
        
        let enabledRules = 
            match getConfig fixParams.Configuration with
            | Ok config -> Some (Configuration.flattenConfig config)
            | _ -> None
        let allRuleNames =
            match enabledRules with
            | Some rules ->  (fun (x:Configuration.LoadedRules) -> ([|
                    x.LineRules.IndentationRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                    x.LineRules.NoTabCharactersRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                    x.LineRules.GenericLineRules |> Array.map (fun rule -> rule.Name)
                    x.AstNodeRules |> Array.map (fun rule -> rule.Name)
                |] |> Array.concat |> Set.ofArray)) rules
            | _ -> Set.empty
        
        if allRuleNames.Contains ruleName then
            linting fileType fixParams target toolsPath true (Some ruleName)
        else
            sprintf "Rule '%s' is not enabled in the configuration." ruleName |> (handleError 1)

    match arguments.GetSubCommand() with
    | Lint lintArgs -> applyLint lintArgs
    | Fix fixArgs -> applySuggestedFix fixArgs
    | _ -> ()

    exitCode
    
/// Must be called only once per process.
/// We're calling it globally so we can call main multiple times from our tests.
let toolsPath = Ionide.ProjInfo.Init.init()

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let parseResults = parser.ParseCommandLine argv
    start parseResults toolsPath
