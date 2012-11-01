namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module Sad =
    let rec zipWith f xs ys =
        match xs, ys with
        | h :: t, h' :: t' -> f h h' :: zipWith f t t'
        | _                -> []

    let sad xs ys = 
        zipWith (-) xs ys
        |> List.map abs
        |> List.sum        
        
    let low = false
    let high = true
    
    let inv a = not a

    let and2 a b = a && b
    let or2 a b = a || b
    let xor2 a b = a <> b
    let xnor2 a b = a = b

    let mux2 sel a b = (sel && b) || (not sel && a)
                                       
    let halfAdd a b = 
        let sum = xor2 a b
        let carry = and2 a b
        (sum, carry)

    let rec bitAdd a bs =
        match bs with
        | [] -> [a]
        | b :: bs -> 
            let (s, c) = halfAdd a b
            s :: bitAdd c bs

    let fullAdd cin a b = 
        let (s0, c0) = halfAdd a b
        let (s1, c1) =  halfAdd cin s0
        (s1, xor2 c0 c1)

    let rec binAdd xs ys 
        = binAdd' low xs ys

    and binAdd' cin xs ys =
        match xs, ys with
        | [], [] -> [cin]
        | _, []  -> bitAdd cin xs
        | [], _  -> bitAdd cin ys
        | h :: t, h' :: t' -> 
            let (sum, cout) = fullAdd cin h h'    
            sum :: binAdd' cout t t'

    let gteCell gin x y = mux2 (xnor2 x y) x gin

    let rec tree f z xs =
        match xs with
        | []           -> z     
        | [x]          -> x
        | x :: y :: ys -> tree f z (ys @ [f x y])

    let orl xs = tree or2 low xs    

    let rec binGte' gin xs ys = 
        match xs, ys with
        | [], [] -> gin
        | _, []  -> orl (gin :: xs)
        | [], _  -> and2 gin (orl ys)
        | h :: t, h' :: t' ->
            let gout = gteCell gin h h'
            binGte' gout t t'

    let binGte xs ys = binGte' high xs ys

    let pad n xs = 
        let m = List.length xs
        if m > n then 
            xs
        else 
            xs @ List.replicate (n-m) false  

    let binDiff xs ys = 
        let xs' = pad (List.length ys) xs
        let ys' = pad (List.length xs) ys
        let gte = binGte xs' ys'
        let xs'' = xs' |> List.map (xor2 (inv gte))
        let ys'' = ys' |> List.map (xor2 gte)
        let rec exceptLast = function 
            | [x] -> []
            | h :: t -> h :: exceptLast t
            | _ -> failwith "empty list"
        exceptLast (binAdd' high xs'' ys'')

    let binSum xs = tree binAdd [] xs
    
    let binSad xs ys =   
        zipWith binDiff xs ys        
        |> binSum    
    
    let bit b = if b then 1 else 0

    let rec num = function
        | [] -> 0
        | b :: bs -> bit b + 2 * num bs

    let binSadProp xs ys =
        sad (List.map num xs) (List.map num ys) = num (binSad xs ys)

    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 3 t
        Console.WriteLine()
        
    let run() = 
        test "binSadProp" binSadProp
    

    

