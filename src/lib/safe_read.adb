with Ada.Containers; use Ada.Containers;

procedure Safe_Read
  (Fd       : int;
   Buf      : out Init_String;
   Has_Read : out ssize_t)
with
  SPARK_Mode
is
   Contents_Pcd_Entry : constant Map :=
     Contents with Ghost;
begin
   loop
      pragma Loop_Invariant (Contents = Contents_Pcd_Entry);
      Read (Fd, Buf, Has_Read);

      if Has_Read > 0 then
         pragma Assert (Elements_Equal_Except (Contents_Pcd_Entry,
                                               Contents,
                                               Fd));
      end if;

      exit when (Has_Read < 0 and then Errors.Get_Errno /= Errors.ADA_EINTR)
                   or else
                 Has_Read >= 0;
   end loop;
end Safe_Read;
