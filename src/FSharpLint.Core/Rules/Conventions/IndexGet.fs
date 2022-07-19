module FSharpLint.Rules.IndexGet

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config = { Type: string }

let runner (config:Config) (args:AstNodeRuleParams) =
    if config.Type.Equals("CSharp") then
        Array.empty
    else
        Array.empty

let rule config =
    { Name = "IndexGet"
      Identifier = Identifiers.IndexGet
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule