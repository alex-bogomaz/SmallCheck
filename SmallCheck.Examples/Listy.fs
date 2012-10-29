namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module Listy =
    let rec isPrefix (xs : list<char>) (ys : list<char>) =
        if xs = [] then true        
        elif ys = [] then false
        else List.head xs = List.head ys || isPrefix (List.tail xs) (List.tail ys)
    
    let isPrefixComplete (xs : list<char>) (ys : list<char>) =
        isPrefix xs (xs @ ys)

    let isPrefixSound (xs : list<char>) (ys : list<char>) = 
        isPrefix xs ys ==>
        exists (fun xs' -> ys = (xs @ xs'))

    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 4 t
        Console.WriteLine()
        
    let run() = 
        test "isPrefix" isPrefix
        test "isPrefixComplete" isPrefixComplete
        test "isPrefixComplete" isPrefixSound        

