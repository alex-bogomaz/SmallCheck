open SmallCheck
open SmallCheck.Examples

//TODO: move this to SmallCheck
TypeClass.InstallSerialInstances<SerialInstances>()
TypeClass.InstallTestableInstances<TestableInstances>()

//Run examples
Listy.run()
Numerical.run()

TypeClass.InstallSerialInstances<Logical.SerialInstances>()
Logical.run()

TypeClass.InstallSerialInstances<BinaryTries.SerialInstances>()
BinaryTries.run()

BitAdd.run()
Sad.run()
