with Lemmas; use Lemmas;

procedure Full_Write
  (Fd        : Int;
   Buf       : Init_String;
   Num_Bytes : Size_T;
   Err       : out Int)
with
  SPARK_Mode => On
is
   Has_Written : ssize_t := 0;
   Has_Written_B : ssize_t;
   Num_Bytes_S : ssize_t := Ssize_T (Num_Bytes);
   Contents_Old : Map (1023, Default_Modulus (1023)) := Contents with Ghost;
   Contents_Pcd_Entry : constant Map (1023, Default_Modulus (1023)) := Contents with Ghost;
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
                          + Natural (Num_Bytes - Size_T (Has_Written)))
                   = Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First - 1 + Natural (Num_Bytes)));

      pragma Assert
        (if Contains (Contents, Fd)
         then Element (Contents_Old, Fd).String
            = Append (Element (Contents_Pcd_Entry, Fd).String, Buf, Has_Written));

      Write (Fd,
             Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
             Num_Bytes - Size_T (Has_Written),
             Has_Written_B);

     Prove_Elements_Equal_Except (Contents, Contents_Old, Contents_Pcd_Entry, Fd);

      if Has_Written_B <= -1 then
         Err := -1;
         return;
      end if;
      pragma Assert (Contains (Contents, Fd));

      if Has_Written_B = 0 then
         Equal_Maps_Implies_Equal_Elements (Contents, Contents_Old, Fd);
         Equal_Implies_Append_Zero
           (Element (Contents, Fd).String,
            Element (Contents_Old, Fd).String,
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
      pragma Assert (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))'Valid_Scalars);

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
        (Element (Contents, Fd).String
         = Append (Element (Contents_Pcd_Entry, Fd).String, Buf, Has_Written));
   end loop;

   Err := 0;
end Full_Write;
