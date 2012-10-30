namespace SmallCheck.Examples

open System
open SmallCheck
open SmallCheck.Common
open SmallCheck.Property
open SmallCheck.Ser
open SmallCheck.Drivers

module Logical =    
    type Name = | P | Q | R

    type Prop = 
        | Var of Name
        | Not of Prop
        | And of Prop * Prop
        | Or  of Prop * Prop
        | Imp  of Prop * Prop        

        override p.ToString() =
            let priority = function 
                | Var _      -> 5
                | Not _      -> 4
                | And (_, _) -> 3
                | Or (_, _)  -> 2
                | Imp (_, _)  -> 1
            let show x = 
                if priority p > priority x then String.Concat("(", x, ")") else x.ToString()

            match p with
            | Var n -> n.ToString()
            | Not q -> "~" + show q
            | And (q, r) -> show q + "&" + show r
            | Or (q, r)  -> show q + "|" + show r
            | Imp (q, r) -> show q + "=>" + show r
    
    let rec eval prop env =
        match prop with
        | Var n -> env n
        | Not q -> not (eval q env)
        | And (q, r) -> eval q env && eval r env
        | Or (q, r)  -> eval q env || eval r env
        | Imp (q, r) -> eval q env <= eval r env

    let rec varsOf = function 
        | Var n -> [n]
        | Not q -> varsOf q
        | And (q, r)
        | Or (q, r) 
        | Imp (q, r) -> varsOf q @ varsOf r

    let envsFor p = 
        let distinctVars = 
            varsOf p |> Seq.distinct |> Seq.toList
        let bind es v =             
            es 
            |> List.map (fun e -> [(fun x -> x = v || e x); e])
            |> List.concat
        List.fold bind [constFunc false] distinctVars

    let tautologous p = List.forall (eval p) (envsFor p)
    
    let satisfiable p = List.exists (eval p) (envsFor p)

    type SerialInstances =     
        static member Name() = 
            { new Serial<Name> with
                member this.series d = cons0 P ||| cons0 Q ||| cons0 R <| d
                member this.coseries rs d =
                    let toNameFunc x y z i = 
                        match i with
                        | P -> x
                        | Q -> y
                        | R -> z                        
                    seq {
                        for x in alts0 rs d do
                        for y in alts0 rs d do
                        for z in alts0 rs d do
                            yield toNameFunc x y z
                    }
            }

        static member Prop() = 
            { new Serial<Prop> with
                member this.series d = cons1 Var ||| cons1 Not ||| consu2 And ||| consu2 Or ||| consu2 Imp <| d
                member this.coseries rs d = null                    
            }

    let taut1 p =
        tautologous p ==> 
        fun e -> eval p e

    let taut2 p =
        not (tautologous p) ==>
        exists (fun e -> not (eval p e))

    let sat1 p e = 
        eval p e ==> 
        satisfiable p

    let sat2 p =
        satisfiable p ==> 
        exists (fun e -> eval p e)

    let tautSat1 p =
        not (tautologous p) ==> 
        satisfiable (Not p)

    let tautSat2 p =
        not (satisfiable p) ==> 
        tautologous (Not p)

    let test (s : string) t =
        Console.WriteLine(s + " ===============================")
        smallCheck 3 t
        Console.WriteLine()
        
    let run() = 
        test "taut1" taut1
        test "taut2" taut2
        test "sat1" sat1
        test "sat2" sat2
        test "tautSat1" tautSat1
        test "tautSat2" tautSat2        