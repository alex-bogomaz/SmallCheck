namespace SmallCheck

module Common =
    let constFunc a _ = a

    let uncurry f = fun (a, b) -> f a b

    let uncurry2 f = fun (a, b, c) -> f a b c
    
    let uncurry3 f = fun (a, b, c, d) -> f a b c d

    let seqOr xs = Seq.fold (fun x y -> x || y) true xs

    let rec zipWith f xs ys =
        match xs, ys with
        | h :: t, h' :: t' -> f h h' :: zipWith f t t'
        | _                -> []

    let take n xs =
        List.toSeq xs
        |> Seq.take n
        |> Seq.toList

    let drop n xs =
        List.toSeq xs
        |> Seq.skip n
        |> Seq.toList
    