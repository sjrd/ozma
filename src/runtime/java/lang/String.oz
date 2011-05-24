functor

import
   OzmaRuntime('NewObject':NewObject
               'IsInstance':IsInstance) at 'x-ozma://root/scala/ozma/OzmaRuntime.ozf'
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
         Object, '<init>#1063877011'(_)
         rawVS := RawVS
         rawString := {ByNeed fun {$} {VirtualString.toString RawVS} end}
         unit
      end

      meth 'toString#1195259493'($)
         self
      end

      meth toRawVS($)
         @rawVS
      end

      meth toRawString($)
         @rawString
      end

      meth '$plus#-918398289'(Right $)
         Raw = (@rawVS)#{Right toRawVS($)}
      in
         {NewObject String StringClass '<init>'(Raw _)}
      end

      meth '$plus#-1324018343'(Right $)
         {self '$plus#-918398289'({Right toString($)} $)}
      end

      meth 'equals#-1875011758'(Other $)
         if {IsInstance Other StringClass} then
            @rawString == {Other toRawString($)}
         else
            false
         end
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
         Object, '<init>#1063877011'(_)
         unit
      end

      meth 'valueOf#-1324018343'(Obj $)
         {Obj toString($)}
      end
   end

   `module:java.lang.String$` = {ByNeed fun {$}
                                           {NewObject `type:java.lang.String$`
                                            `class:java.lang.String$`
                                            '<init>'(_)}
                                        end}

   `class:java.lang.String$` = {ByNeed fun {$}
                                          {NewObject Class ClassClass
                                           '<init>'("java.lang.String$"
                                                    ObjectClass
                                                    nil
                                                    [ObjectClass]
                                                    _)}
                                       end}

end
