namespace SmallCheck

open Common
open Ser
open Testable 
open System



module Property =    
    type Property = 
        | Property of (int -> seq<TestCase>)        

    let test<'a> =
        TypeClass.Test<'a>()

    type InvocationResult =
        | Ok of seq<TestCase>
        | Exception of Exception

    let safeTest f x d =
        try
            Ok (test (f x) d)
        with ex ->                
            Exception ex
    
    let forAll (ser : int -> seq<'a>) f =                
        let res d = 
            seq {
                for x in ser d do      
                    match safeTest f x d with
                    | Ok results -> 
                        for r in results do yield TestCase(r.Result, x.ToString() :: r.Arguments)
                    | Exception ex -> 
                        yield TestCase(TestResult.Exception ex, [ x.ToString() ])
                    
            }
        Property(res)

    let forAllElem xs = forAll (constFunc xs)

    let boolToResult b = if b then Pass else Fail

    let existence unique (ser : int -> seq<'a>) f =        
        let existenceDepth d =             
            let resultIsOk (tc : TestCase) = not (tc.Result = Fail)        
            let witnesses =            
                [ for x in ser d do                
                    if Seq.forall resultIsOk (test (f x) d) then
                        yield x.ToString()
                ]
            let valid = 
                let len = witnesses.Length 
                if unique then len = 1 else len > 0
                
            let arguments = 
                if valid then []
                elif witnesses.Length = 0 then ["non-existence"]
                //else "non-uniqueness" :: (witnesses.Head :: (witnesses.Tail.Head :: List.empty)) //TODO: !!! no 'take' for lists
                else "non-uniqueness" :: witnesses
                    
            seq { yield TestCase(boolToResult valid, arguments) }

        Property(existenceDepth)
                
    let thereExists ser f = existence false ser f

    let thereExists1 ser f = existence true ser f
    
    let thereExistsElem xs = thereExists (constFunc xs)

    let thereExists1Elem xs = thereExists1 (constFunc xs)

    let exists f = thereExists series f

    let exists1 f = thereExists1 series f

    let existsDeeperBy f = thereExists (f >> series)

    let exists1DeeperBy f = thereExists1 (f >> series)
    
    let (==>) cond x =        
        if cond then 
            Property (test x)
        else
            let nothing = seq { yield TestCase(Inappropriate, []) }
            Property (constFunc nothing)

