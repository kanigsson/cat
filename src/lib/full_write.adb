with Ada.Containers; use Ada.Containers;
with Lemmas;

with Safe_Write;
procedure Full_Write
  (Fd        : int;
   Buf       : Init_String;
   Num_Bytes : size_t;
   Err       : out int)
with
  SPARK_Mode => On
is
   Has_Written : ssize_t := 0;
   Has_Written_B : ssize_t;
   Num_Bytes_S : ssize_t := ssize_t (Num_Bytes);
   Has_Written_Old : ssize_t;
   Contents_Old : Map :=
     Contents with Ghost;
   Contents_Pcd_Entry : constant Map :=
     Contents with Ghost;
begin
   while Has_Written /= Num_Bytes_S loop
      Contents_Old := Contents;
      Has_Written_Old := Has_Written;
      pragma Assert (Elements_Equal_Except
                       (Contents_Pcd_Entry,
                        Contents,
                        Fd));

      pragma Assert (Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First + Integer (Has_Written) - 1
                          + Natural (Num_Bytes - size_t (Has_Written)))
                   = Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First - 1 + Natural (Num_Bytes)));

      pragma Assert
        (if Has_Key (Contents, Fd)
         then
           Is_Append (Get (Contents_Pcd_Entry, Fd),
                      Buf,
                      Get (Contents_Old, Fd),
                      Has_Written));

      Safe_Write (Fd,
             Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
             Num_Bytes - size_t (Has_Written),
             Has_Written_B);

      pragma Assert (Elements_Equal_Except
                              (Contents_Pcd_Entry,
                               Contents,
                               Fd));

      if Has_Written_B = -1 then
         Err := -1;
         return;
      end if;
      pragma Assert (Has_Key (Contents, Fd));

      Has_Written := Has_Written + Has_Written_B;

      pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
      pragma Assert
        (Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Initialized);
      pragma Assert
        (for all J in Buf'First .. Buf'First - 1 + Natural (Has_Written) =>
           (J in Buf'First .. Buf'First - 1 + Natural (Num_Bytes)
              and then Buf (J)'Initialized));
      pragma Assert
        (Buf (Buf'First
               ..
              Buf'First - 1 + Natural (Has_Written))'Initialized);

      Lemmas.Lemma_Is_Append_Trans
        (Get (Contents_Pcd_Entry, Fd), Get (Contents_Old, Fd),
         Get (Contents, Fd), Buf, Has_Written_Old, Has_Written_B);
      pragma Loop_Invariant (Elements_Equal_Except
                              (Contents_Pcd_Entry,
                               Contents,
                               Fd));

      pragma Loop_Invariant (Has_Written in 0 .. Num_Bytes_S);
      pragma Loop_Invariant (Has_Key (Contents, Fd));
      pragma Loop_Invariant (Same_Keys
                              (Contents_Pcd_Entry,
                               Contents));
      pragma Loop_Invariant
        (Is_Append (Get (Contents_Pcd_Entry, Fd), Buf,
                    Get (Contents, Fd), Has_Written));
   end loop;

   Err := 0;
end Full_Write;
