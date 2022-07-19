module FSharpLint.Rules.IndexGet

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (args:AstNodeRuleParams) =
    let styleType = "CSharp"
    if styleType.Equals("CSharp") then
        Array.empty
    else
        Array.empty

let rule =
    { Name = "IndexGet"
      Identifier = Identifiers.IndexGet
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule