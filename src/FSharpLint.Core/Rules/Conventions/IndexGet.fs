module FSharpLint.Rules.IndexGet

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion

[<RequireQualifiedAccess>]
type Config = {
    Type:string
}

let runner (config:Config) (args:AstNodeRuleParams) =
    let styleType = config.Type
    printfn "%A" args.AstNode
    if styleType.Equals("OCaml") then
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _
                , SynExpr.App (ExprAtomicFlag.Atomic, _, SynExpr.Ident _, SynExpr.ArrayOrListOfSeqExpr (_,expr,range),_), _, _) ->
                { Range = range; Message = "error found";SuggestedFix = None;TypeChecks = []} |> Array.singleton
            | _ ->
                Array.empty
        | _ -> 
            Array.empty
    else
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _
                , SynExpr.DotIndexedGet (_,_,_,range), _, _) ->
                { Range = range; Message = "error found";SuggestedFix = None;TypeChecks = []} |> Array.singleton
            | _ ->
                Array.empty
        | _ -> 
            Array.empty

let rule config =
    { Name = "IndexGet"
      Identifier = Identifiers.IndexGet
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
