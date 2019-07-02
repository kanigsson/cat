with Ada.Containers; use Ada.Containers;
with Lemmas;         use Lemmas;

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
   Contents_Old : Map (OPEN_MAX - 1, Default_Modulus (OPEN_MAX - 1)) :=
     Contents with Ghost;
   Contents_Pcd_Entry : constant Map (OPEN_MAX - 1,
                                      Default_Modulus (OPEN_MAX - 1)) :=
     Contents with Ghost;
begin
   while Has_Written /= Num_Bytes_S loop
      Contents_Old := Contents;
      pragma Assert (M.Elements_Equal_Except
                       (Model (Contents),
                        Model (Contents_Pcd_Entry),
                        Fd));

      pragma Assert (Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First + Integer (Has_Written) - 1
                          + Natural (Num_Bytes - size_t (Has_Written)))
                   = Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First - 1 + Natural (Num_Bytes)));

      pragma Assert
        (if Contains (Contents, Fd)
         then Element (Contents_Old, Fd)
            = Append (Element (Contents_Pcd_Entry, Fd),
                      Buf,
                      Has_Written));

      Safe_Write (Fd,
             Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
             Num_Bytes - size_t (Has_Written),
             Has_Written_B);

      Prove_Elements_Equal_Except (Contents,
                                   Contents_Old,
                                   Contents_Pcd_Entry,
                                   Fd);

      if Has_Written_B = -1 then
         Err := -1;
         return;
      end if;
      pragma Assert (Contains (Contents, Fd));

      if Has_Written_B = 0 then
         Equal_Maps_Implies_Equal_Elements (Contents, Contents_Old, Fd);
         Equal_Implies_Append_Zero
           (Element (Contents, Fd),
            Element (Contents_Old, Fd),
            Buf,
            Has_Written,
            Has_Written_B);
      end if;

      Prove_Loop_Invariant
        (Contents, Contents_Old, Contents_Pcd_Entry,
         Buf,
         Has_Written, Has_Written_B, Num_Bytes_S,
         Fd);
      Has_Written := Has_Written + Has_Written_B;

      pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
      pragma Assert
        (Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars);
      pragma Assert
        (for all J in Buf'First .. Buf'First - 1 + Natural (Has_Written) =>
           (J in Buf'First .. Buf'First - 1 + Natural (Num_Bytes)
              and then Buf (J)'Valid_Scalars));
      pragma Assert
        (Buf (Buf'First
               ..
              Buf'First - 1 + Natural (Has_Written))'Valid_Scalars);

      pragma Loop_Invariant (M.Elements_Equal_Except
                              (Model (Contents),
                               Model (Contents_Pcd_Entry),
                               Fd));

      pragma Loop_Invariant (Has_Written in 0 .. Num_Bytes_S);
      pragma Loop_Invariant (Contains (Contents, Fd));
      pragma Loop_Invariant (M.Same_Keys
                              (Model (Contents_Pcd_Entry),
                               Model (Contents)));
      pragma Loop_Invariant
        (Element (Contents, Fd)
         = Append (Element (Contents_Pcd_Entry, Fd), Buf, Has_Written));
   end loop;

   Err := 0;
end Full_Write;
