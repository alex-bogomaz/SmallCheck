open SmallCheck
open SmallCheck.Examples

//TODO: move this to SmallCheck
TypeClass.InstallSerialInstances<SerialInstances>()
TypeClass.InstallTestableInstances<TestableInstances>()


//Examples from Haskell Smallcheck
Listy.run()
Numerical.run()

TypeClass.InstallSerialInstances<Logical.SerialInstances>()
Logical.run()

TypeClass.InstallSerialInstances<BinaryTries.SerialInstances>()
BinaryTries.run()

BitAdd.run()
Sad.run()

//Exception examples
Exceptions.run()