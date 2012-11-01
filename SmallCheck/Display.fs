namespace SmallCheck

open System

type Display() =
    static let isFuncType (t : Type) = 
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ -> _>    

    static let rec asFuncType (t : Type) = 
        if t <> null then 
            if isFuncType t then t else asFuncType t.BaseType
        else
            null

    static let isFunc r =        
        asFuncType (r.GetType()) <> null
    
    static let showFunc r (i : int) =        
        let genMeth = typeof<Display>.GetMethod("ShowFunc").GetGenericMethodDefinition()
        let args = asFuncType(r.GetType()).GetGenericArguments()
        let meth = genMeth.MakeGenericMethod(args)
        meth.Invoke(null, [| r; i |]) :?> string

    static let ident i = String.replicate (i + 2) " "

    static let show r i = 
        if isFunc r then 
            showFunc r i
        else
            ident i + (r.ToString())

    static member Show r = show r 0
                    
    static member ShowFunc(f : 'a -> 'b, i) = 
        let depthLimit = 3
        let vals = 
            Ser.series<'a> depthLimit 
            |> Seq.map (fun a -> String.Concat(ident i, a, "->", Environment.NewLine, show (f a) (i + 1)))
            |> Seq.toArray
            
        String.Join(Environment.NewLine, vals)

