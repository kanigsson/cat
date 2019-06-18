with Lemmas; use Lemmas;
procedure Safe_Write
  (Fd          : int;
   Buf         : Init_String;
   Num_Bytes   : Size_T;
   Has_Written : out ssize_t)
with
  SPARK_Mode
is
   Contents_Pcd_Entry : constant Map (1023, Default_Modulus (1023)) := Contents with Ghost;
   Fd_Content_Old : Unbounded_String :=
     (if Contains (Contents, Fd) then Element (Contents, Fd).String else Null_Unbounded_String) with Ghost;
begin
   loop
      pragma Loop_Invariant (Contents = Contents_Pcd_Entry);
      Fd_Content_Old :=
        (if Contains (Contents, Fd) then Element (Contents, Fd).String else Null_Unbounded_String);

      Write (Fd, Buf, Num_Bytes, Has_Written);

      if Has_Written > 0 then
         pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
         pragma Assert (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))'Valid_Scalars);
         pragma Assert (M.Elements_Equal_Except (Model (Contents), Model (Contents_Pcd_Entry), Fd));
         Equal_And_Append
            (Element (Contents, Fd).String,
             Fd_Content_Old,
             Element (Contents_Pcd_Entry, Fd).String,
             Buf,
             Has_Written);
      end if;

      exit when (Has_Written < 0 and then Errors.Get_Errno /= Errors.ADA_EINTR) or else Has_Written >= 0;
   end loop;
end Safe_Write;
