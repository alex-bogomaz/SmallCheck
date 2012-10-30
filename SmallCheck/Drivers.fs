namespace SmallCheck

open System
open Testable
open Ser
open Property

module Drivers = 
    let putLine (s : string) =
        Console.WriteLine(s)
    
    let check (rs : seq<TestCase>) =
        let mutable n = 0
        let mutable notMeet = 0
        let mutable ok = true        
        use iter = rs.GetEnumerator()
        while ok && iter.MoveNext() do
            let res = iter.Current            
            if res.Result = Inappropriate then              
                n <- n + 1
                notMeet <- notMeet + 1
            elif res.Result = Pass then
                n <- n + 1
            else
                putLine ("  Failed test no. " + (n + 1).ToString() + ". Test values follow.")
                res.Arguments 
                |> List.iter (fun a -> putLine ("  " + a.ToString())) //TODO: show functions. Show arguments in one line
                ok <- false
        
        if ok then        
            putLine ("  Completed " + n.ToString() + " test(s)")
            putLine (if ok then " without failure." else ".")
            if (notMeet > 0) then putLine ("  But " + notMeet.ToString() + " did not meet ==> condition.")

        ok           
    
    let iterCheck lower upper t = 
        let rec iter d = 
            putLine ("Depth " + d.ToString() + ":")
            let ok = check (test t d)
            if ok && d < upper then 
                iter (d+1)
        iter lower

    let smallCheck d p = iterCheck 0 d p