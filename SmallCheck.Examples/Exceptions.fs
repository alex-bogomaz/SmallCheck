namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module Exceptions = 

    let zeroDiv1 (x : int) = 
        2 / x = x

    let zeroDiv2 (x : int) (y : int) = 
        let _ = 2 / (x + 2) + y
        true

    let zeroDiv3 (x : int) = 
        1 / (x + 2) > 0 ==> true    

    let zeroDivUnit (x : int) = 
        let _ = 2 / (x + 3)
        ()

    let unitProp (x : int) = ()

    let zeroDivInExists = exists (fun (x : int) -> 1 / x)
    
    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 3 t
        Console.WriteLine()
        
    let run() = 
        test "zeroDiv1" zeroDiv1
        test "zeroDiv2" zeroDiv2
        test "zeroDiv3" zeroDiv3
        test "zeroDivUnit" zeroDivUnit
        test "unitProp" unitProp
        test "zeroDivInExists" zeroDivInExists
        

