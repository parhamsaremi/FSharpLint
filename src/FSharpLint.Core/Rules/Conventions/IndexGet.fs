module FSharpLint.Rules.IndexGet

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion

let runner (args:AstNodeRuleParams) =
    let styleType = "CSharp"
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
        Array.empty

let rule =
    { Name = "IndexGet"
      Identifier = Identifiers.IndexGet
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule