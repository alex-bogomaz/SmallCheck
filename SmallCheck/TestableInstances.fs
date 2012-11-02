namespace SmallCheck

open Testable
open Ser
open Property
open Common

type TestableInstances =
    static member Bool() = 
        { new Testable<bool> with
            member this.test b _ = seq { yield TestCase(boolToResult b, [])  }
        }
    
    static member Func() = 
        { new Testable<'a -> 'b> with
            member this.test f d = 
                let (Property p) = forAll series f
                p d
        }

    static member Property() = 
        { new Testable<Property> with
            member this.test (Property p) d = p d
        }
   