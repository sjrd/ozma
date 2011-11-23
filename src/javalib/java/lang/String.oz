functor

import
   OzmaRuntime('NewObject':NewObject
               'IsInstance':IsInstance
               'ModuleAccessor':ModuleAccessor) at 'x-ozma://system/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':Object
                              'class:java.lang.Object':ObjectClass) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':Class
                             'class:java.lang.Class':ClassClass) at 'x-ozma://root/java/lang/Class.ozf'

export
   'type:java.lang.String':String
   'class:java.lang.String':StringClass
   'type:java.lang.String$':`type:java.lang.String$`
   'module:java.lang.String$':`module:java.lang.String$`
   'class:java.lang.String$':`class:java.lang.String$`

define

   class String from Object
      attr
         rawVS
         rawString

      meth '<init>'(RawVS $)
         Object, '<init>():java.lang.Object'(_)
         rawVS := RawVS
         rawString := {ByNeed fun {$} {VirtualString.toString RawVS} end}
         unit
      end

      meth 'toString():java.lang.String'($)
         self
      end

      meth toRawVS($)
         @rawVS
      end

      meth toRawString($)
         @rawString
      end

      meth '$plus(java.lang.String):java.lang.String'(Right $)
         Raw = (@rawVS)#{Right toRawVS($)}
      in
         {NewObject String StringClass '<init>'(Raw _)}
      end

      meth '$plus(java.lang.Object):java.lang.String'(Right $)
         {self '$plus(java.lang.String):java.lang.String'({Right toString($)} $)}
      end

      meth 'equals(java.lang.Object):scala.Boolean'(Other $)
         if {IsInstance Other StringClass} then
            @rawString == {Other toRawString($)}
         else
            false
         end
      end

      meth 'length():scala.Int'($)
         {Length @rawString}
      end
   end

   StringClass = {ByNeed fun {$}
                            {NewObject Class ClassClass
                             '<init>'("java.lang.String"
                                      ObjectClass
                                      nil
                                      [ObjectClass]
                                      _)}
                         end}

   class `type:java.lang.String$` from Object
      meth '<init>'($)
         Object, '<init>():java.lang.Object'(_)
         `modulevar~java.lang.String$` = self
         unit
      end

      meth 'valueOf(java.lang.Object):java.lang.String'(Obj $)
         {Obj toString($)}
      end
   end

   `modulevar~java.lang.String$`
   `module:java.lang.String$` = {ModuleAccessor `modulevar~java.lang.String$`
                                 `type:java.lang.String$`
                                 `class:java.lang.String$`
                                 '<init>'(_)}

   `class:java.lang.String$` = {ByNeed fun {$}
                                          {NewObject Class ClassClass
                                           '<init>'("java.lang.String$"
                                                    ObjectClass
                                                    nil
                                                    [ObjectClass]
                                                    _)}
                                       end}

end
