module FSharpLint.Rules.IndexerAccessorStyleConsistency

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion
open System

[<RequireQualifiedAccess>]
type Config = {
    Style: string
}

let generateOutput (range: FSharp.Compiler.Text.Range) (styleMain: string) (styleError: string) =
    { Range = range
      Message = sprintf "You must use %s styling instead of %s for indexer accessing" styleMain styleError
      SuggestedFix = None
      TypeChecks = List.Empty } |> Array.singleton

let runner (config: Config) (args: AstNodeRuleParams) =
    let styleType = config.Style
    if String.Equals (styleType, "ocaml", StringComparison.InvariantCultureIgnoreCase) then
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _
                , SynExpr.App (ExprAtomicFlag.Atomic, _, SynExpr.Ident _, SynExpr.ArrayOrListOfSeqExpr (_, expr, range), _), _, _) ->
                generateOutput range "OCaml" "CSharp"
            | _ ->
                Array.empty
        | _ -> 
            Array.empty
    elif String.Equals (styleType, "csharp", StringComparison.InvariantCultureIgnoreCase) then
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _
                , SynExpr.DotIndexedGet (_, _, _, range), _, _) ->
                generateOutput range "CSharp" "OCaml"
            | _ ->
                Array.empty
        | _ -> 
            Array.empty
    else
        failwith (sprintf "Unknown style type %s" styleType)

let rule config =
    { Name = "IndexerAccessorStyleConsistency"
      Identifier = Identifiers.IndexerAccessorStyleConsistency
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
