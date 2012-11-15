namespace SmallCheck

open Ser
open Common

type SerialInstances =     
    static member Unit() = 
        { new Serial<unit> with
            member this.series d = seq { yield () }
            member this.coseries rs d =
                let toResult r = fun () -> r
                rs d |> Seq.map toResult
        }

    static member Int() = 
        { new Serial<int> with
            member this.series d = 
                let ss = seq { -d .. d }
                ss
            member this.coseries rs d = 
                let toIntFunc pos neg zer i =
                    if i > 0 then pos (uint32(i - 1))
                    elif i < 0 then neg (uint32(abs i - 1))
                    else zer
                seq {
                    for pos in alts1 rs d do
                    for neg in alts1 rs d do
                    for zer in alts0 rs d do
                        yield toIntFunc pos neg zer
                }            
        }

    static member UInt() = 
        { new Serial<uint32> with
            member this.series d = seq { 0 .. d } |> Seq.map uint32
            member this.coseries rs d = 
                let toUIntFunc pos zer i =
                    if i > 0u then pos (i - 1u)
                    else zer
                seq {
                    for pos in alts1 rs d do                    
                    for zer in alts0 rs d do
                        yield toUIntFunc pos zer
                }
        }


    //TODO: provide proper implementation (need encodeFloat, decodeFloat)
    static member Float() = 
        { new Serial<float> with
            member this.series d =
                let toFloat (a, b) = (float a)*(2.0 ** (float b))
                series d |> Seq.map toFloat
            member this.coseries rs d = null                
        }
    
    static member Char() = 
        { new Serial<char> with
            member this.series d = seq { 'a' .. 'z' } |> Seq.take (d + 1)
            member this.coseries rs d = 
                let ord c = (uint32) c - (uint32) 'a'
                coseries rs d |> Seq.map (fun f ->  ord >> f)
        }
    
    static member Bool() = 
        { new Serial<bool> with
            member this.series d = cons0 true ||| cons0 false <| d 
            member this.coseries rs d = 
                seq {
                    for r1 in rs d do
                    for r2 in rs d do
                        yield (fun b -> if b then r1 else r2)
                }
        }
        
    static member Option<'a>() = 
        { new Serial<'a option> with
            member this.series d = cons0 None ||| cons1 Some <| d 
            member this.coseries rs d = 
                let toOptionFunc r f opt =                     
                    match opt with
                    | None -> r
                    | Some(x) -> f x
                seq {
                    for f in alts1 rs d do
                    for r in alts0 rs d do
                        yield toOptionFunc r f
                }
        }
    
    static member List<'a>() =
        { new Serial<'a list> with
            member this.series d = cons0 [] ||| consu2 List.Cons <| d
            member this.coseries rs d = 
                let toListFunc y f xs =                    
                    match xs with
                    | [] -> y
                    | (h::t) -> f h t
                seq {
                    for y in alts0 rs d do
                    for f in alts2 rs d do
                        yield toListFunc y f
                }
        }
    
    static member Seq<'a>() =
        { new Serial<'a seq> with        
            member this.series d = 
                let prepend h t = 
                    seq { yield h
                          yield! t }
                cons0 Seq.empty ||| cons2 prepend <| d 

            member this.coseries rs d = null
        }
    
    static member Tuple2() = 
        { new Serial<'a * 'b> with
            member this.series d = series >< series <| d            
            member this.coseries rs d = 
                coseries (coseries rs) d |> Seq.map uncurry
        }

    static member Tuple3() = 
        { new Serial<'a * 'b * 'c> with
            member this.series d = 
                let flatten (x, (y, z)) = (x, y, z)
                series d |> Seq.map flatten 

            member this.coseries rs d = 
               coseries (coseries (coseries rs)) d |> Seq.map uncurry2
        }

    static member Tuple4() = 
        { new Serial<'a * 'b * 'c * 'd> with
            member this.series d = 
                let flatten (w, (x, (y, z))) = (w, x, y, z)
                series d |> Seq.map flatten 

            member this.coseries rs d = 
                coseries (coseries (coseries (coseries rs))) d |> Seq.map uncurry3
        }


    static member Func() = 
        { new Serial<'a -> 'b> with
            member this.series d = coseries series d       
            member this.coseries rs d  =
                let args = series d |> Seq.toList
                let rec nest xs _ = 
                    match xs with
                    | []     -> rs d |> Seq.map (fun c -> fun [] -> c)
                    | h :: t -> coseries (nest t) d |> Seq.map (fun f -> fun (b :: bs) -> f b bs)

                nest args d 
                |> Seq.toList
                |> List.map (fun g -> fun f -> g (List.map f args)) |> List.toSeq
        }



