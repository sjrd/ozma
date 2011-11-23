functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Byte$':Module) at 'x-ozma://root/java/lang/Byte.ozf'

export
   'type:scala.runtime.VolatileByteRef':Type
   'class:scala.runtime.VolatileByteRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Byte" true Module Type Class}
end
