% This implementation is entirely copied from the book
% Concepts, Techniques, and Models of Computer Programming
% by Peter Van Roy and Seif Haridi
functor

export
   NewMonitor

define

   %%%%%%%%%%%%%%%%%%%%
   % Concurrent queue %
   %%%%%%%%%%%%%%%%%%%%

   fun {NewQueue}
      X
      C = {NewCell q(0 X X)}
      L = {NewLock}

      fun {Size}
         lock L then
            @C.1
         end
      end

      proc {Insert X}
         N S E1
      in
         lock L then
            q(N S X|E1) = @C
            C := q(N+1 S E1)
         end
      end

      fun {Delete}
         N S1 E X
      in
         lock L then
            q(N X|S1 E) = @C
            C := q(N-1 S1 E)
         end
         X
      end

      fun {DeleteAll}
         lock L then
            X S E
         in
            q(_ S E) = @C
            C := q(0 X X)
            E = nil
            S
         end
      end

      fun {DeleteNonBlock}
         lock L then
            if {Size} > 0 then
               [{Delete}]
            else
               nil
            end
         end
      end
   in
      queue(insert:Insert delete:Delete size:Size
            deleteAll:DeleteAll deleteNonBlock:DeleteNonBlock)
   end

   %%%%%%%%%%%%%%%%%%%%
   % Get-release lock %
   %%%%%%%%%%%%%%%%%%%%

   fun {NewGRLock}
      Token1 = {NewCell unit}
      Token2 = {NewCell unit}
      CurThread = {NewCell unit}

      proc {GetLock}
         if {Thread.this} \= @CurThread then
            Old New
         in
            {Exchange Token1 Old New}
            {Wait Old}
            Token2 := New
            CurThread := {Thread.this}
         end
      end

      proc {ReleaseLock}
         CurThread := unit
         unit = @Token2
      end
   in
      'lock'(get:GetLock release:ReleaseLock)
   end

   %%%%%%%%%%%
   % Monitor %
   %%%%%%%%%%%

   fun {NewMonitor}
      Queue = {NewQueue}
      Lock = {NewGRLock}

      fun {LockM P}
         {Lock.get}
         try
            {P}
         finally
            {Lock.release}
         end
      end

      proc {WaitM}
         X
      in
         {Queue.insert X}
         {Lock.release}
         {Wait X}
         {Lock.get}
      end

      proc {NotifyM}
         U = {Queue.deleteNonBlock}
      in
         case U of [X] then
            X = unit
         else
            skip
         end
      end

      proc {NotifyAllM}
         L = {Queue.deleteAll}
      in
         for X in L do
            X = unit
         end
      end
   in
      monitor('lock':LockM wait:WaitM
              notify:NotifyM notifyAll:NotifyAllM)
   end

end
