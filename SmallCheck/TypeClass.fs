namespace SmallCheck

open System
open System.Reflection
open System.Collections.Generic
open Testable

type TypeClass() =
    static let mutable serialInstances = new Dictionary<Type, MethodInfo>()
    static let mutable testableInstances = new Dictionary<Type, MethodInfo>()

    static let installInstances (t : Type) (instances : Dictionary<Type, MethodInfo>) = 
        t.GetMethods((BindingFlags.Static ||| BindingFlags.Public))    
        |> Array.iter (TypeClass.InstallInstance instances)
            
    static let typeDef (t : Type) =
        if t.IsGenericType then t.GetGenericTypeDefinition() else t
    
    static member InstallSerialInstances<'a>() =
        installInstances typeof<'a> serialInstances        

    static member InstallTestableInstances<'a>() =
        installInstances typeof<'a> testableInstances                
    
    static member private InstallInstance instances meth =
        let instanceType = meth.ReturnType.GetGenericArguments().[0]        
        instances.Add(typeDef instanceType, meth)
                           
    static member private InstanceImpl<'a> (instances : Dictionary<Type, MethodInfo>) =
        let instanceType = typeof<'a>
        let instanceTypeDef = typeDef instanceType
        let instanceImplMethod = 
            if instanceType.IsGenericType then 
                instances.[instanceTypeDef].MakeGenericMethod(instanceType.GetGenericArguments())
            else 
                instances.[instanceTypeDef]
                        
        //TODO: cache instances
        instanceImplMethod.Invoke(null, Array.empty)

    static member Series<'a>()  =         
        (TypeClass.InstanceImpl<'a> serialInstances :?> Serial<'a>).series

    static member Coseries<'a, 'b>() : (int -> seq<'b>) -> int -> seq<'a -> 'b> =
        (TypeClass.InstanceImpl<'a> serialInstances :?> Serial<'a>).coseries        

    static member Test<'a>()  =         
        (TypeClass.InstanceImpl<'a> testableInstances :?> Testable<'a>).test
    
        

    

