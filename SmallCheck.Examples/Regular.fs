namespace SmallCheck.Examples

open System
open SmallCheck
open SmallCheck.Property
open SmallCheck.Ser
open SmallCheck.Drivers


//TODO: not finished yet
module Regular = 
    type RE =
        | Emp
        | Lam
        | Sym of char
        | Alt of list<RE>
        | Cat of list<RE>
        | Rep of RE
                
        override p.ToString() = 
            let showBrackIf p (x : RE) =
                let showStr s = if p x then s else ""                
                String.Concat(showStr "(", x, showStr ")")
            let isAlt = function
                | Alt _ -> true
                | _ -> false
            let isCat = function
                | Cat _ -> true
                | _ -> false
            match p with
            | Emp    -> "0"
            | Lam    -> "1"
            | Sym c  -> c.ToString()
            | Alt xs -> String.Join("+", xs)
            | Cat xs -> String.Join("", List.map (showBrackIf isAlt) xs)
            | Rep x  -> showBrackIf (fun x -> isCat x || isAlt x) x + "*"
    
    let cat xs =
        let catList = function
            | Cat ys -> ys
            | z -> [z]        
        match xs with
        | [] -> Lam    
        | [x] -> x
        | _ -> Cat (List.map catList xs |> List.concat)

    let alt xs =
        let altList = function
            | Alt ys -> ys
            | z -> [z]
        match xs with
        | [] -> Lam    
        | [x] -> x
        | _ -> Alt (List.map altList xs |> List.concat)

    type SerialInstances =             
        static member RE() = 
            { new Serial<RE> with
                member this.series d = 
                    cons0 Emp ||| cons0 Lam
                    ||| cons1 Sym << depth 0
                    ||| cons1 alt ||| cons1 cat ||| cons1 Rep <| d
                member this.coseries rs d = null                    
            }
