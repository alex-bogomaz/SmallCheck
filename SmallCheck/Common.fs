namespace SmallCheck

module internal Common =
    let constFunc a _ = a

    let uncurry f = fun (a, b) -> f a b

    let uncurry2 f = fun (a, b, c) -> f a b c
    
    let uncurry3 f = fun (a, b, c, d) -> f a b c d

    let seqOr xs = Seq.fold (fun x y -> x || y) true xs


