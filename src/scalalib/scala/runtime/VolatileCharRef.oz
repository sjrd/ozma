functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Character$':Module) at 'x-ozma://root/java/lang/Character.ozf'

export
   'type:scala.runtime.VolatileCharRef':Type
   'class:scala.runtime.VolatileCharRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Char" true Module Type Class}
end
