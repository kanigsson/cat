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

   ---------------------
   --  String lemmas  --
   ---------------------

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
       Natural (Bytes) <= Natural'Last - Length (Left_1)
         and then
       Buf (Buf'First.. Buf'First - 1 + Natural (Bytes))'Valid_Scalars
         and then
       Str = Append (Left_1, Buf, Bytes),
     Post =>
       Str = Append (Left_2, Buf, Bytes);
    procedure Equal_And_Append
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : Int)
    is null;

    procedure Equal_And_Append
      (Str, Left, Right_1, Right_2 : Unbounded_String)
    with
      Pre  =>
        Right_1 = Right_2
          and then
        Length (Right_1) <= Natural'Last - Length (Left)
          and then
        Str = Left & Right_1,
      Post => Str = Left & Right_2;
   procedure Equal_And_Append
      (Str, Left, Right_1, Right_2 : Unbounded_String)
   is null;

   procedure Prove_Equality
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Read, Has_Written                      : ssize_t;
      Input, Stdout                              : Int) with
   Pre  =>
     Has_Read = Has_Written
       and then
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
     Length (Element (Contents_Pcd_Entry, Stdout).String)
             <= Natural'Last - Length (Element (Contents_Old, Input).String)
       and then
     Element (Contents_Old, Stdout).String
     = Element (Contents_Pcd_Entry, Stdout).String
     & Element (Contents_Old, Input).String
       and then
     NAtural (Has_Written) <= Natural'Last - Length (Element (Contents_Old, Stdout).String)
       and then
     Element (Contents, Stdout).String
     = Append (Element (Contents_Old, Stdout).String, Buf, Has_Written)
       and then
     NAtural (Has_Read) <= Natural'Last - Length (Element (Contents_Old, Input).String)
       and then
     Element (Contents, Input).String
     = Append (Element (Contents_Old, Input).String, Buf, Has_Read),

     Post =>
       Element (Contents, Stdout).String
     = Element (Contents_Pcd_Entry, Stdout).String
     & Element (Contents, Input).String;
   procedure Prove_Equality
     (Contents,	Contents_Old, Contents_Pcd_Entry : Map;
      Buf     	                                 : Init_String;
      Has_Read, Has_Written                      : ssize_t;
      Input, Stdout                              : Int)
   is null;
end Lemmas;
