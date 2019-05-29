with Iostr; use Iostr;
with Contents_Table_Type; use Contents_Table_Type;
with INterfaces.C; use Interfaces.C;

package Lemmas with
  Ghost,
  SPARK_Mode => On
is
   use Ghost_Package;
   use FOrmal_Maps;
   use FOrmal_Maps.Formal_Model;

   procedure Equal_String
     (Str_1, Str_2, Left, Right : Unbounded_String)
   with
     Pre =>
       Str_1 = Str_2
         and then
       Str_2 = Left & Right,
     Post => Str_1 = Left & Right;

   procedure Equal_String
     (Str_1, Str_2, Left, Right : Unbounded_String)
   is null;

   procedure Equal_And_Append
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : Int)
   with
     Pre =>
       Bytes in 0 .. Buf'Length
         and then
       Left_1 = Left_2
         and then
       Buf (Buf'First.. Buf'First - 1 + Natural (Bytes))'Valid_Scalars
         and then
       Str = Append (Left_1, Buf (Buf'First .. Buf'First - 1 + NAtural (Bytes)), Bytes),
     Post =>
       Str = Append (Left_2, Buf, Bytes);


   procedure Equal_And_Append
     (Str, Left, Right_1, Right_2 : Unbounded_String)
   with
     Pre  =>
       Right_1 = Right_2
         and then
       Str = Left & Right_1,
     Post => Str = Left & Right_2;
   procedure Equal_And_Append
      (Str, Left, Right_1, Right_2 : Unbounded_String)
   is null;



   procedure Prove_Equality
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Read                                   : ssize_t;
      Input, Stdout                              : Int) with
   Pre  =>
     Has_Read in 0 .. Buf'Length
       and then
     Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
       and then
     Contains (Contents, Input)
       and then
     Contains (Contents, Stdout)
       and then
     M.Same_Keys (Model (Contents), Model (Contents_Old))
       and then
     M.Same_Keys (Model (Contents), MOdel (Contents_Pcd_Entry))
       and then
     Element (Contents_Old, Stdout).String
     = Element (Contents_Pcd_Entry, Stdout).String
     & Element (Contents_Old, Input).String
       and then
     Element (Contents, Stdout).String
     = Append (Element (Contents_Old, Stdout).String, Buf, Has_Read)
       and then
     Element (Contents, Input).String
     = Append (Element (Contents_Old, Input).String, Buf, Has_Read),

     Post =>
       Element (Contents, Stdout).String
     = Element (Contents_Pcd_Entry, Stdout).String
     & Element (Contents, Input).String;
end Lemmas;
