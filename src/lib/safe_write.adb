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
begin
   loop
      pragma Loop_Invariant (Contents = Contents_Pcd_Entry);

      Write (Fd, Buf, Num_Bytes, Has_Written);

      if Has_Written > 0 then
         pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
         pragma Assert
           (Buf (Buf'First
                  ..
                 Buf'First - 1 + Natural (Has_Written))'Valid_Scalars);
         pragma Assert (Elements_Equal_Except (Contents,
                                               Contents_Pcd_Entry,
                                               Fd));
      end if;

      exit when (Has_Written < 0 and then Errors.Get_Errno /= Errors.ADA_EINTR)
                   or else
                 Has_Written >= 0;
   end loop;
end Safe_Write;
