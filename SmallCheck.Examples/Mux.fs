namespace SmallCheck.Examples

open System
open SmallCheck.Common
open SmallCheck.Property
open SmallCheck.Drivers

module Mux = 
    let rec transpose (xs : 'a list list) =
        if xs.Head <> [] then
            List.map List.head xs :: transpose (List.map List.tail xs)
        else []

    let rec tree f xs =
        match xs with        
        | [x]          -> x
        | x :: y :: ys -> tree f (ys @ [f x y])

    let rec decode = function
        | [] -> [true]
        | [x] -> [not x; x]
        | h :: t -> 
            decode t
            |> List.map (fun y -> [not h && y; h && y])
            |> List.concat  

    let unaryMux sel xs =
        let t = zipWith (fun s x -> List.map (fun y -> s && y) x) sel xs
        List.map (tree (||)) (transpose t)

    let binaryMux sel xs  = unaryMux (decode sel) xs

    let bitMux2 sel x y = (sel && y) || (not sel && x)

    let muxf5 =  bitMux2

    let muxf6 =  bitMux2

    let busMux2 sel xs ys = zipWith (bitMux2 sel) xs ys
    
    let rec log2 n =
        let half = n / 2
        if half = 0 then 0 else 1 + log2 half

    let pad n xs = 
        let m = List.length xs
        if m > n then 
            xs
        else 
            xs @ List.replicate (n-m) false  

    let rec bitMux8 sels xs =
        match sels, xs with
        | _, [x] ->  x
        | s0 :: _, [x0; x1] -> bitMux2 s0 x0 x1
        | s0 ::s1 :: _, [x0; x1; x2; x3] -> muxf5 s1 (bitMux8 [s0] [x0; x1]) (bitMux8 [s0] [x2; x3])
        | s0 :: s1 :: s2 :: _, [x0; x1; x2; x3; x4; x5; x6; x7] ->
            muxf6 s2 (bitMux8 [s0; s1] [x0; x1; x2; x3]) (bitMux8 [s0; s1] [x4; x5; x6; x7])
        | _ ->             
            let n = log2 (List.length xs)
            let m = 1 <<< n
            bitMux8 (take n sels) (pad m xs)

    let rec groupn n xs =
        match xs with
        | [] -> []
        | _  -> take n xs :: groupn n (drop n xs)

    let rec bitMux (sels : list<bool>) xs =
        match xs with
        | [x] ->  x
        | _   -> 
            let ttttt = List.replicate 10 (take 3 sels) //!!!!!!!!! no replicate
            let ys = zipWith bitMux8 ttttt (groupn 8 xs) 
            bitMux (drop 3 sels) ys  

    let binaryMux' sel xs = List.map (bitMux sel) (transpose xs)

    let bit b = if b then 1 else 0

    let rec num = function
        | [] -> 0
        | b :: bs -> bit b + 2 * num bs

    (*let prop_mux0 sel xs =  
        List.length xs = 2 ^ length sel
                     && all ((== length (head xs)) . length) xs
                    ==> binaryMux sel xs == xs !! num sel
    *)
    



