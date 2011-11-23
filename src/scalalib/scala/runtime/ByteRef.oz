functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Byte$':Module) at 'x-ozma://root/java/lang/Byte.ozf'

export
   'type:scala.runtime.ByteRef':Type
   'class:scala.runtime.ByteRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Byte" false Module Type Class}
end
