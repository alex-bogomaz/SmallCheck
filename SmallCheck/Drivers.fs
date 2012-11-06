namespace SmallCheck

open System
open Testable
open Ser
open Property

module Drivers =     
    let putStr (s : string) =
        Console.Write(s)

    let putStrLn (s : string) =
        Console.WriteLine(s)

    let printArguments (res : TestCase) =
        for a in res.Arguments do putStrLn a

    let printException ex =
        putStrLn ("  Exception information follow.") //TODO: print inner exceptions
        putStrLn ("  "+ ex.ToString())
                
    let check (rs : seq<TestCase>) =
        let mutable n = 0
        let mutable notMeet = 0
        let mutable ok = true        
        use iter = rs.GetEnumerator()
        while ok && iter.MoveNext() do
            let res = iter.Current
            match res.Result with            
            | Inappropriate ->
                n <- n + 1
                notMeet <- notMeet + 1
            | Pass -> n <- n + 1
            | Fail ->
                putStrLn ("  Failed test no. " + (n + 1).ToString() + ". Test values follow.")
                printArguments res
                ok <- false
            | TestResult.Exception ex ->            
                putStrLn ("  Exception in test no. " + (n + 1).ToString() + ". Test values follow.")
                printArguments res
                printException ex
                ok <- false
        
        if ok then        
            putStr ("  Completed " + n.ToString() + " test(s)")
            putStrLn (if ok then " without failure." else ".")
            if (notMeet > 0) then putStrLn ("  But " + notMeet.ToString() + " did not meet ==> condition.")

        ok           
    
    let iterCheck lower upper t = 
        let rec iter d = 
            putStrLn ("Depth " + d.ToString() + ":")
            let ok = check (test t d)
            if ok && d < upper then 
                iter (d+1)
        iter lower

    let smallCheck d p = iterCheck 0 d p