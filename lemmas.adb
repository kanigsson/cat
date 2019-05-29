package body Lemmas with
  SPARK_Mode => On
is

   procedure Equal_And_Append
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : Int)
   is
   begin
      pragma Assert (Left_1 = Left_2);
      pragma Assert (Append (Left_1, Buf, Bytes) = Append (Left_2, Buf, Bytes));
   end Equal_And_Append;

   procedure Prove_Equality
     (Contents,	Contents_Old, Contents_Pcd_Entry : Map;
      Buf     	                                 : Init_String;
      Has_Read                                   : ssize_t;
      Input, Stdout                              : Int)
   is
   begin
      pragma Assert
        (Append
           (Element (Contents_Pcd_Entry, Stdout).String & Element (Contents_Old, Input).String,
            Buf,
            Has_Read)
         = Element (Contents_Pcd_Entry, Stdout).String
         & Element (Contents, Input).String);
   end Prove_Equality;
end Lemmas;
