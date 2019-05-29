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
   Contents_Pcd_Entry : Map (1023, Default_Modulus (1023)) := Contents with Ghost;

   procedure Prove_Elements_Equal_Except with
     Ghost,
     Pre =>
       (M.Elements_Equal_Except
         (Model (Contents),
          Model (Contents_Old),
          Fd)
          or else
        Contents = Contents_Old)
          and then
       M.Elements_Equal_Except
         (Model (Contents_Old),
          Model (Contents_Pcd_Entry),
          Fd),
      Post =>
        M.Elements_Equal_Except
          (Model (Contents),
           Model (Contents_Pcd_Entry),
           Fd);
   procedure Prove_Elements_Equal_Except is null;
begin
   pragma Assert (Buf'Valid_Scalars);
   while Has_Written /= Num_Bytes_S loop
      Contents_Old := Contents;
      pragma Assert (M.Elements_Equal_Except
                       (Model (Contents),
                        Model (Contents_Pcd_Entry),
                        Fd));
      pragma Assert (Buf'Valid_Scalars);
      pragma Assert (Buf (Natural (Has_Written) + 1 .. Buf'Last)'Valid_Scalars);
      pragma Assert (Has_Written in 0 .. Num_Bytes_S - 1);
      pragma Assert (Num_Bytes - Size_T (Has_Written) in 1 .. Num_Bytes);
      pragma Assert (Num_Bytes <= buf'Length and Buf (Natural (Has_Written) + 1 .. Buf'Last)'Valid_Scalars);
      Write (Fd,
             Buf (Natural (Has_Written) + 1 .. Buf'Last),
             Num_Bytes - Size_T (Has_Written),
             Has_Written_B);

      Prove_Elements_Equal_Except;

      if Has_Written_B <= -1 then
        Err := -1;
        return;
      end if;

      Has_Written := Has_Written + Has_Written_B;

      pragma Assert
        (Buf'Valid_Scalars);

      pragma Loop_Invariant
        (Element (Contents, Fd).String
         = Append (Element (Contents_Pcd_Entry, Fd).String, Buf, Has_Written));

      pragma Loop_Invariant (M.Same_Keys
                              (Model (Contents_Pcd_Entry),
                               Model (Contents)));
      pragma Loop_Invariant (M.Elements_Equal_Except
                              (Model (Contents),
                               Model (Contents_Pcd_Entry),
                               Fd));

      pragma Loop_Invariant (Has_Written in 0 .. Num_Bytes_S);
   end loop;

   Err := 0;
end Full_Write;
