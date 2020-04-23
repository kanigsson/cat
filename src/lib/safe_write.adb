with Ada.Containers; use Ada.Containers;

procedure Safe_Write
  (Fd          : int;
   Buf         : Init_String;
   Num_Bytes   : size_t;
   Has_Written : out ssize_t)
with
  SPARK_Mode
is
   Contents_Pcd_Entry : constant Map := Contents with Ghost;
   --  Stores the value of Content at the beginning of the procedure
begin
   loop
      pragma Loop_Invariant (Contents = Contents_Pcd_Entry);
      --  If we begin a new iteration, then this means that Contents is equal
      --  to its value at the beginning of the procedure.

      Write (Fd, Buf, Num_Bytes, Has_Written);
      --  Call to Write

      if Has_Written > 0 then
         pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
         pragma Assert
           (Buf (Buf'First
                  ..
                 Buf'First - 1 + Natural (Has_Written))'Initialized);
      end if;
      --  The Ghost code above is used to prove the postcondition

      exit when (Has_Written < 0 and then Errors.Get_Errno /= Errors.ADA_EINTR)
                   or else
                 Has_Written >= 0;
      --  The procedure returns only when we wrote effectively or an error
      --  different from EINTR occured.
   end loop;
end Safe_Write;
