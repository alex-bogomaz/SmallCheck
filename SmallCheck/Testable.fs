namespace SmallCheck

module Testable =
    type TestResult =
        | Pass
        | Fail
        | Inappropriate
    type TestCase (result : TestResult, arguments : list<string>) =
        member this.Result = result
        member this.Arguments = arguments    

    type Testable<'a> =    
        abstract member test : 'a -> int -> seq<TestCase>

