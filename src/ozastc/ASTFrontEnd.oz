% Front-end for the AST compiler
% @author sjrd

functor

import
   Open

export
   ParseASTFile
   ParseASTVirtualString

define

   TYPE_INT    = 1
   TYPE_FLOAT  = 2
   TYPE_ATOM   = 3
   TYPE_STRING = 4
   TYPE_TUPLE  = 5
   TYPE_LIST   = 6
   TYPE_UNIT   = 7
   TYPE_TRUE   = 8
   TYPE_FALSE  = 9

   fun {ReadASTFromFile File}
      fun {ReadBytes Count}
         {File read(list:$ size:Count)}
      end

      fun {ReadInt}
         {StringToInt {ReadString}}
      end

      fun {ReadFloat}
         {StringToFloat {ReadString}}
      end

      fun {ReadAtom}
         {StringToAtom {ReadString}}
      end

      fun {ReadLength}
         [Byte1 Byte0] = {ReadBytes 2}
      in
         Byte1*256 + Byte0
      end

      fun {ReadString}
         Len = {ReadLength}
      in
         {ReadBytes Len}
      end

      proc {ReadTuple ?Result}
         Label = {ReadAtom}
         Width = {ReadLength}
      in
         if Width == 0 then
            Result = Label
         else
            Result = {MakeTuple Label Width}
            for I in 1..Width do
               Result.I = {ReadTypedValue}
            end
         end
      end

      fun {ReadList}
         fun {ReadListTail Len}
            if Len == 0 then
               nil
            else
               {ReadTypedValue}|{ReadListTail Len-1}
            end
         end
      in
         {ReadListTail {ReadLength}}
      end

      fun {ReadUnit}
         unit
      end

      fun {ReadTrue}
         true
      end

      fun {ReadFalse}
         false
      end

      ReadFunByType = functions(TYPE_INT:ReadInt
                                TYPE_FLOAT:ReadFloat
                                TYPE_ATOM:ReadAtom
                                TYPE_STRING:ReadString
                                TYPE_TUPLE:ReadTuple
                                TYPE_LIST:ReadList
                                TYPE_UNIT:ReadUnit
                                TYPE_TRUE:ReadTrue
                                TYPE_FALSE:ReadFalse)

      fun {ReadTypedValue}
         [Type] = {ReadBytes 1}
      in
         {ReadFunByType.Type}
      end
   in
      [{ReadTypedValue}]
   end

   fun {ParseASTFile FileName Reporter TestSwitch Macros}
      File = {New Open.file init(name:FileName flags:[read])}
      Result
   in
      try
         Result = {ReadASTFromFile File}
      finally
         {File close}
      end
      Result
   end

   fun {ParseASTVirtualString VS Reporter TestSwitch Macros}
      raise notImplemented end
   end

end
