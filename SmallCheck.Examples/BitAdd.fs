namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module BitAdd =
    let and2 (a, b) = a && b

    let xor2 (a, b) = a <> b

    let halfAdd (a, b) = 
        let sum = xor2 (a, b)
        let carry = and2 (a, b)
        (sum, carry)  

    let bit b = if b then 1 else 0

    let rec num = function
        | [] -> 0
        | b :: bs -> bit b + 2 * num bs
        
    let rec bitAdd a bs =
        match bs with
        | [] -> [a]
        | b :: bs -> 
            let (s, c) = halfAdd (a, b)
            s :: bitAdd c bs

    let bitAddProp a bs = 
        num (bitAdd a bs) = bit a + num bs

    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 8 t
        Console.WriteLine()
        
    let run() = 
        test "bitAddProp" bitAddProp

