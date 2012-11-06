namespace SmallCheck

open System

module Testable =
    type TestResult =
        | Pass
        | Fail
        | Inappropriate
        | Exception of Exception

    type TestCase (result : TestResult, arguments : list<string>) =
        member this.Result = result
        member this.Arguments = arguments    

    type Testable<'a> =    
        abstract member test : 'a -> int -> seq<TestCase>

