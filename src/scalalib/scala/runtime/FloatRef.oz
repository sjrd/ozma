functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Float$':Module) at 'x-ozma://root/java/lang/Float.ozf'

export
   'type:scala.runtime.FloatRef':Type
   'class:scala.runtime.FloatRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Float" false Module Type Class}
end
