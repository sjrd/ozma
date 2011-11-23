functor

import
   OzmaRuntime('NewObject':NewObject
               'InitObject':InitObject
               'StringLiteral':StringLiteral) at 'x-ozma://system/OzmaRuntime.ozf'
   ObjectMonitor(newMonitor:NewMonitor) at 'x-ozma://system/ObjectMonitor.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':Class
                             'class:java.lang.Class':ClassClass) at 'x-ozma://root/java/lang/Class.ozf'

export
   'type:java.lang.Object':Object
   'class:java.lang.Object':ObjectClass

define

   NextID = {NewCell 1}

   class Object from BaseObject
      attr
         identity
         'class'
         monitor

      meth !InitObject(ObjClass InitMessage)
         local ID Next in
            {Exchange NextID ID Next}
            Next = ID + 1
            identity := ID
         end

         'class' := ObjClass
         monitor := {ByNeed NewMonitor}
         {self InitMessage}
      end

      meth '<init>():java.lang.Object'($)
         unit
      end

      meth '$getPublic$'(Field $)
         @Field
      end

      meth '$setPublic$'(Field Value)
         Field := Value
      end

      meth 'getClass():java.lang.Class'($)
         @'class'
      end

      % For convenience in other Oz functors
      meth getClass($)
         {self 'getClass():java.lang.Class'($)}
      end

      % In the JavaDoc, defined as:
      %   getClass().getName() + '@' + Integer.toHexString(hashCode())
      meth 'toString():java.lang.String'($)
         ClassName = {{@'class' 'getName():java.lang.String'($)} toRawString($)}
         HashCode = {self 'hashCode():scala.Int'($)}
      in
         {StringLiteral ClassName#'@'#HashCode}
      end

      % For convenience in other Oz functors
      meth toString($)
         {self 'toString():java.lang.String'($)}
      end

      meth identity($)
         @identity
      end

      meth 'hashCode():scala.Int'($)
         @identity
      end

      meth 'equals(java.lang.Object):scala.Boolean'(Other $)
         self == Other
      end

      meth synchronized(P $)
         {@monitor.'lock' P}
      end

      meth 'wait():scala.Unit'($)
         {@monitor.wait}
         unit
      end

      meth 'notify():scala.Unit'($)
         {@monitor.notify}
         unit
      end

      meth 'notifyAll():scala.Unit'($)
         {@monitor.notifyAll}
         unit
      end
   end

   ObjectClass = {ByNeed fun {$}
                            {NewObject Class ClassClass
                             '<init>'("java.lang.Object"
                                      unit
                                      nil
                                      nil
                                      _)}
                         end}

end
