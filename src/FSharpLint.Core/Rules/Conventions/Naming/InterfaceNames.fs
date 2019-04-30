module FSharpLint.Rules.InterfaceNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) ->
        let isNotTypeExtension =
            match typeDef with
            | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> false
            | _ -> true

        if isNotTypeExtension then
            match componentInfo with
            | SynComponentInfo.ComponentInfo(attrs, _, _, identifier, _, _, _, _) ->
                match List.tryLast identifier with
                | Some typeIdentifier ->
                    if not (isMeasureType attrs) && isInterface typeDef then
                        typeIdentifier |> Array.singleton
                    else
                        Array.empty
                | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { name = "InterfaceNames" 
      identifier = Identifiers.InterfaceNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule
