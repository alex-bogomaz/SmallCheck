namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module Listy =    
    let foldl1 = List.reduce //TODO: check this
    let foldr1 = List.reduceBack
    let rec span p xs = 
        match xs with
        | [] -> ([], [])
        | x :: xs' ->  
            let (ys, zs) = span p xs'
            if p x then (x::ys, zs) else ([], xs)        

    let elem x xs = 
        List.tryFind (fun y -> y = x) xs <> None 
            
    let fold1 (xs : list<bool>) =        
        not (List.isEmpty xs) ==>        
        fun f -> foldl1 f xs = foldr1 f xs

    let fold2 (xs : list<bool>) (ys : list<bool>) =
        (not (List.isEmpty xs) && not (List.isEmpty ys)) ==>
        fun f -> f (foldr1 f xs) (foldr1 f ys) = foldr1 f (xs @ ys)

    let span1 (xs : list<bool>) (ys : list<bool>) (zs : list<bool>) =
        xs @ ys = zs ==> 
        exists(fun t -> (xs, ys) = span t zs)

    let union1 (xs : list<bool>) (ys : list<bool>) =
        exists (fun zs ->
            fun b -> (elem b zs) = (elem b xs || elem b ys)
        )

    let union2 (xs : list<bool>) (ys : list<bool>) =
        existsDeeperBy (fun d -> d * 2) (fun zs ->
            fun b -> (elem b zs) = (elem b xs || elem b ys)
        )

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
        test "fold1" fold1
        test "fold2" fold1
        test "span1" span1
        test "union1" union1
        test "union2" union2
        test "isPrefix" isPrefix
        test "isPrefixComplete" isPrefixComplete
        test "isPrefixComplete" isPrefixSound        

