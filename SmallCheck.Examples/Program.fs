open SmallCheck
open SmallCheck.Examples

//TODO: move this to SmallCheck
TypeClass.InstallSerialInstances<SerialInstances>()
TypeClass.InstallTestableInstances<TestableInstances>()

Listy.run()
Numerical.run()

let _ = System.Console.ReadLine()

