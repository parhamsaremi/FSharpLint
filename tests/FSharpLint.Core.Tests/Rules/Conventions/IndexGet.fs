module FSharpLint.Core.Tests.Rules.Conventions.IndexGet

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsIndexGet() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(IndexGet.rule)

    [<Test>]
    member this.IndexGetCSharpStyleWhenUsingOCaml() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.ErrorsExist
    
    [<Test>]
    member this.IndexGetOCamlStyleWhenUsingCSharp() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.ErrorsExist
    
    [<Test>]
    member this.IndexGetCSharpStyleWhenUsingCSharp() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.IndexGetOCamlStyleWhenUsingOCaml() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist
