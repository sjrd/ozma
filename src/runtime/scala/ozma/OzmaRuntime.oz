functor

import
   `functor:java.lang.String`('type:java.lang.String':`type:java.lang.String`
                              'class:java.lang.String':`class:java.lang.String`) at '../../java/lang/String.ozf'
   `functor:java.lang.ClassCastException`('type:java.lang.ClassCastException':`type:java.lang.ClassCastException`
                                          'class:java.lang.ClassCastException':`class:java.lang.ClassCastException`) at '../../java/lang/ClassCastException.ozf'

export
   'InitObject':InitObject
   'NewObject':NewObject
   'IsInstance':IsInstance
   'AsInstance':AsInstance
   'StringLiteral':StringLiteral

define

   InitObject = {NewName}

   fun {NewObject Type Class Init}
      {New Type InitObject(Class Init)}
   end

   fun {IsInstance Obj Class}
      {Class 'isInstance#1063877011'(Obj $)}
   end

   fun {AsInstance Obj Class}
      if {IsInstance Obj Class} then
         Obj
      else
         raise
            {NewObject `type:java.lang.ClassCastException`
             `class:java.lang.ClassCastException`
             '<init>'(_)}
         end
      end
   end

   fun {StringLiteral RawString}
      {NewObject `type:java.lang.String`
       `class:java.lang.String`
       '<init>'(RawString _)}
   end

end
