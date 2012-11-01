namespace SmallCheck.Examples

open System
open SmallCheck.Property
open SmallCheck.Drivers

module Numerical =                
    let recipRecip x = 
        1.0 / (1.0 / x) = x    

    let logExp x = exp (log x) = x
    
    let primes =         
        let rec primes m =
            seq {
                let isPrime = 
                    seq { 2u .. uint32 (sqrt(float m)) }
                    |> Seq.forall (fun x -> m % x <> 0u)

                if isPrime then yield m
                yield! primes (m + 1u)
            }
        seq { 
            yield 2u
            yield! primes 3u
        }

    let primes1 n =
        let firstPrimes m = Seq.take m primes
        n > 1u ==> forAll firstPrimes (fun p -> p % n > 0u || n = p)

    let primes2 n =
        let product xs = List.fold (fun x y -> x * y) 1u xs 
        let power (p, (e : uint32)) = product (List.replicate (int e) p)
        let expProp (expsSeq : seq<uint32>) =
            let exponents = Seq.toList expsSeq
            let firstPrimes = Seq.take exponents.Length primes |> Seq.toList
            (exponents = [] || (List.rev exponents).Head <> 0u) && n = product (List.zip firstPrimes exponents |> List.map power)

        n > 0u ==> exists1 expProp

    
    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 7 t
        Console.WriteLine()
        
    let run() = 
        test "recipRecip" recipRecip
        test "logExp" logExp
        test "primes1" primes1
        test "primes2" primes2

    
                
