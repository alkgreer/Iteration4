import bookExamples.chpt15._

val l1: Service.Log   = new ConsoleLogger    // ERROR: No Service "value"
val l2: Service1.Log  = new ConsoleLogger    // ERROR: No Service1 "value"
val l3: Service#Log   = new ConsoleLogger    // ERROR: Type mismatch
val l4: Service1#Log  = new ConsoleLogger    // Works!
