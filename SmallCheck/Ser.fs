namespace SmallCheck

open Common

module Ser =    
    let series<'a> =
        TypeClass.Series<'a>()

    let coseries<'a, 'b> =
        TypeClass.Coseries<'a, 'b>()

    let concat (s1 : int -> seq<'a>) (s2 : int -> seq<'a>) = 
        fun d -> Seq.append (s1 d) (s2 d)
        
    let prod (s1 : int -> seq<'a>) (s2 : int -> seq<'b>) = 
        fun d -> 
            seq {
                    for x in s1 d do
                    for y in s2 d do
                        yield (x, y)
                }

    let (|||) s1 s2 = concat s1 s2
    let (><) s1 s2 = prod s1 s2

    let cons0 c = 
        fun (d : int) -> seq { yield c }

    let cons1 c =
        fun d -> 
            seq { if d > 0 then 
                        for x in series (d - 1) do yield c x  
                }
    
    let cons2 c =
        fun d -> 
            seq { if d > 0 then 
                    for (x, y) in series (d - 1) do 
                        yield c x y 
                }

    let consu2 c =
        fun d -> 
            seq { if d > 0 then 
                    for (x, y) in series (d - 1) do 
                        yield c (x, y)
                }

    let cons3 c =
        fun d -> 
            seq { if d > 0 then 
                    for (x, y, z) in series (d - 1) do 
                        yield c x y z
                }

    let cons4 c =
        fun d -> 
            seq { if d > 0 then 
                    for (w, x, y, z) in series (d - 1) do 
                        yield c w x y z
                }

    let alts0 rs d = rs d
    
    let alts1 rs d = 
        if d > 0 then
            coseries rs (d - 1)
        else rs d |> Seq.map constFunc
    
    let alts2 rs d = 
        let toConst r _ _ = r
        if d > 0 then             
            coseries (coseries rs) (d - 1)
        else rs d |> Seq.map toConst        


