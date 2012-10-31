namespace SmallCheck.Examples

open System
open SmallCheck
open SmallCheck.Property
open SmallCheck.Drivers
open SmallCheck.Ser

module BinaryTries =
    type BT1 = 
        | E 
        | B of bool * BT1 * BT1
    
    let rec contains1 bt xs = 
        match bt, xs with
        | E, _                    -> false
        | B (b, _, _), []         -> b
        | B (_, z, _), false :: s -> contains1 z s
        | B (_, _, o), true :: s  -> contains1 o s

    let (===) f g = fun x -> f x = g x

    let uniqueBT1 f =
        exists1DeeperBy (fun d -> d + 1) (fun bt -> contains1 bt === f)

    type BT2 =
        | E2 
        | NE of BT2'          
    and BT2' = 
        | T 
        | O of bool * BT2' 
        | I of bool * BT2' 
        | OI of bool * BT2' * BT2'

    type SerialInstances =             
        static member BT1() = 
            { new Serial<BT1> with
                member this.series d = cons0 E ||| consu3 B <| d
                member this.coseries rs d = null                    
            }

        static member BT2() = 
            { new Serial<BT2> with
                member this.series d = cons0 E2 ||| cons1 NE <| d
                member this.coseries rs d = null                    
            }

        static member BT2'() = 
            { new Serial<BT2'> with
                member this.series d = cons0 T ||| consu2 O ||| consu2 I ||| consu3 OI <| d
                member this.coseries rs d = null                    
            }
        
    let rec convert' = function 
        | T              -> B (true, E, E)
        | O (b, z')      -> B (b, convert' z', E)
        | I (b, o')      -> B (b, E, convert' o')
        | OI (b, o', z') -> B (b, convert' z', convert' o')

    let convert = function
        | E2 -> E
        | NE bt' -> convert' bt'

    let contains2 = convert >> contains1

    let uniqueBT2 f =
        exists1DeeperBy (fun d -> d + 1) (fun bt -> contains2 bt === f)

    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 2 t
        Console.WriteLine()
        
    let run() = 
        test "uniqueBT1" uniqueBT1
        test "uniqueBT2" uniqueBT2




