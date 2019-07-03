with Ada.Containers; use Ada.Containers;
with Lemmas;         use Lemmas;

procedure Safe_Read
  (Fd       : int;
   Buf      : out Init_String;
   Has_Read : out ssize_t)
with
  SPARK_Mode
is
   Contents_Pcd_Entry : constant Map (OPEN_MAX - 1,
                                      Default_Modulus (OPEN_MAX - 1)) :=
     Contents with Ghost;
   Fd_Content_Old : Unbounded_String :=
     (if Contains (Contents, Fd)
      then Element (Contents, Fd)
      else Null_Unbounded_String) with Ghost;
begin
   loop
      pragma Loop_Invariant (Contents = Contents_Pcd_Entry);
      Fd_Content_Old :=
        (if Contains (Contents, Fd)
         then Element (Contents, Fd)
         else Null_Unbounded_String);

      Read (Fd, Buf, Has_Read);

      if Has_Read > 0 then
         pragma Assert (M.Elements_Equal_Except (Model (Contents),
                                                 Model (Contents_Pcd_Entry),
                                                 Fd));
         Equal_And_Append
            (Element (Contents, Fd),
             Fd_Content_Old,
             Element (Contents_Pcd_Entry, Fd),
             Buf,
             Has_Read);
      end if;

      exit when (Has_Read < 0 and then Errors.Get_Errno /= Errors.ADA_EINTR)
                   or else
                 Has_Read >= 0;
   end loop;
end Safe_Read;
