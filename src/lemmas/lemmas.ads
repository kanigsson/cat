with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;

--  In this package, the comment "Prove preconditions" is often present in
--  preconditions of a procedure. This means that the preconditions above
--  the comment are used to prove checks (preconditions, range checks)
--  on the precondition lines below it.

package Lemmas with
  Ghost,
  SPARK_Mode => On
is
   use Ghost_Package;
   use Formal_Maps;
   use Formal_Maps.Formal_Model;

   --  If Str_1 = Str_2 and Str_2 = Left & Right, then Str_1 = Left & Right
   procedure Equality_Transitivity
     (Str_1, Str_2, Left, Right : Unbounded_String)
   with
     Pre =>
       Str_1 = Str_2
         and then
       Str_2 = Left & Right,
     Post => Str_1 = Left & Right;
   procedure Equality_Transitivity
     (Str_1, Str_2, Left, Right : Unbounded_String)
   is null;

   --  It is possible to substitute the left argument in call to Append.
   procedure Left_Substitution_In_Concat
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : int)
   with
     Pre =>
       Bytes in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Bytes))'Valid_Scalars
       --  Prove preconditions

         and then
       Left_1 = Left_2
         and then
       Str = Append (Left_1, Buf, Bytes),
     Post =>
       Str = Append (Left_2, Buf, Bytes);

   --  It is possible to substitute the right argument in call to "&".
   procedure Right_Substitution_In_Concat
     (Str, Left, Right_1, Right_2 : Unbounded_String)
   with
     Pre  =>
       Right_1 = Right_2
         and then
       Str = Left & Right_1,
     Post => Str = Left & Right_2;
   procedure Right_Substitution_In_Concat
      (Str, Left, Right_1, Right_2 : Unbounded_String)
   is null;

   --  This procedure helps proving a loop invariant in Copy_To_Stdout
   --  (in src/cat.adb)
   procedure Prove_Copy_To_Stdout_LI
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Read                                   : ssize_t;
      Input, Stdout                              : int) with
   Pre  =>
     Has_Read in 0 .. Buf'Length
       and then
     Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
       and then
     Contains (Contents, Input)
       and then
     Contains (Contents, Stdout)
       and then
     M.Same_Keys (Model (Contents), Model (Contents_Old))
       and then
     M.Same_Keys (Model (Contents), Model (Contents_Pcd_Entry))
     --  Prove preconditions

       and then
     Element (Contents_Old, Stdout)
     = Element (Contents_Pcd_Entry, Stdout)
     & Element (Contents_Old, Input)
       and then
     Element (Contents, Stdout)
     = Append (Element (Contents_Old, Stdout), Buf, Has_Read)
       and then
     Element (Contents, Input)
     = Append (Element (Contents_Old, Input), Buf, Has_Read),

     Post =>
       Element (Contents, Stdout)
     = Element (Contents_Pcd_Entry, Stdout)
     & Element (Contents, Input);

   --  This procedure helps proving a loop invariant in Full_Write
   --  (in src/lib/full_write.adb)
   procedure Prove_Full_Write_LI
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S    : ssize_t;
      Fd                                         : int) with
     Pre =>
       Num_Bytes_S in 1 .. ssize_t (Integer'Last)
         and then Integer (Num_Bytes_S) <= Buf'Length
         and then Buf'Last < Natural'Last
         and then Has_Written in 0 .. Num_Bytes_S
         and then Has_Written_B in 0 .. Num_Bytes_S - Has_Written
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes_S))'Valid_Scalars
         and then Contains (Contents, Fd)
         and then Contains (Contents_Old, Fd)
         and then Contains (Contents_Pcd_Entry, Fd)
         --  Prove preconditions

         and then
       Element (Contents_Old, Fd)
       = Append (Element (Contents_Pcd_Entry, Fd), Buf, Has_Written)
         and then
       Element (Contents, Fd)
       = Append (Element (Contents_Old, Fd),
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B),
     Post =>
       Element (Contents, Fd)
       = Append (Element (Contents_Pcd_Entry, Fd),
                 Buf,
                 Has_Written + Has_Written_B);

end Lemmas;
