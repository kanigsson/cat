with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;

package Lemmas with
  Ghost,
  SPARK_Mode => On
is
   use Ghost_Package;
   use Formal_Maps;
   use Formal_Maps.Formal_Model;

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

   procedure Substit
     (A, B, C, D : Unbounded_String;
      Buf        : Init_String;
      Has_Read   : ssize_t)
   with
     Pre =>
       Has_Read in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
         and then
       A = Append (B, Buf, Has_Read)
         and then
       B = C & D,
     Post => A = Append (C & D, Buf, Has_Read);

   procedure Substit_2
     (A, B, C  : Unbounded_String;
      Buf      : Init_String;
      Has_Read : ssize_t)
   with
     Pre =>
       Has_Read in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
         and then
       A = Append (B & C, Buf, Has_Read),
     Post => A = B & Append (C, Buf, Has_Read);

   procedure Substit_3
     (A, B, C, D : Unbounded_String;
      Buf        : Init_String;
      Has_Read   : ssize_t)
   with
     Pre =>
       Has_Read in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
         and then
       A = B & Append (C, Buf, Has_Read)
         and then
       Append (C, Buf, Has_Read) = D,
     Post => A = B & D;
   procedure Substit_3
     (A, B, C, D : Unbounded_String;
      Buf        : Init_String;
      Has_Read   : ssize_t)
   is null;

   procedure Equal_And_Append
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : int)
   with
     Pre =>
       Bytes in 0 .. Buf'Length
         and then
       Left_1 = Left_2
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Bytes))'Valid_Scalars
         and then
       Str = Append (Left_1, Buf, Bytes),
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

   procedure Equal_Implies_Append_Zero
      (Str_1, Str_2               : Unbounded_String;
       Buf                        : Init_String;
       Has_Written, Has_Written_B : ssize_t)
   with
     Pre =>
       Str_1 = Str_2
         and then Has_Written_B = 0
         and then Integer (Has_Written) in 0 .. Buf'Length
         and then Buf'Last < Natural'Last,
     Post =>
       Str_1
       = Append (Str_2,
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B);
   procedure Equal_Implies_Append_Zero
      (Str_1, Str_2               : Unbounded_String;
       Buf                        : Init_String;
       Has_Written, Has_Written_B : ssize_t)
   is null;

   procedure Equal_Maps_Implies_Equal_Elements
      (Contents, Contents_Old : Map;
       Fd                     : int)
   with
     Pre =>
       Contents = Contents_Old
         and then Contains (Contents, Fd),
     Post => Element (Contents, Fd).String = Element (Contents_Old, Fd).String;
   procedure Equal_Maps_Implies_Equal_Elements
      (Contents, Contents_Old : Map;
       Fd                     : int)
   is null;

   procedure Equal_Maps_Implies_Elements_Equal_Except
     (Contents, Contents_Old : Map;
      Fd_1, Fd_2             : int)
   with
     Pre  =>
       Contents = Contents_Old,
     Post =>
       M.Elements_Equal_Except (Model (Contents),
                                Model (Contents_Old),
                                Fd_1,
                                Fd_2);
   procedure Equal_Maps_Implies_Elements_Equal_Except
     (Contents, Contents_Old : Map;
      Fd_1, Fd_2             : int) is null;

   procedure Prove_Equality
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

   procedure Prove_Elements_Equal_Except
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Fd                                         : int)
   with
     Pre =>
       (M.Elements_Equal_Except
         (Model (Contents),
          Model (Contents_Old),
          Fd)
          or else
        Contents = Contents_Old)
          and then
       (M.Elements_Equal_Except
         (Model (Contents_Old),
          Model (Contents_Pcd_Entry),
          Fd)
          or else
        Contents_Old = Contents_Pcd_Entry),
      Post =>
        M.Elements_Equal_Except
          (Model (Contents),
           Model (Contents_Pcd_Entry),
           Fd);
   procedure Prove_Elements_Equal_Except
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Fd                                         : int)
   is null;

   procedure Prove_Loop_Invariant
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
         and then
       Element (Contents_Old, Fd).String
       = Append (Element (Contents_Pcd_Entry, Fd).String, Buf, Has_Written)
         and then
       Element (Contents, Fd).String
       = Append (Element (Contents_Old, Fd).String,
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B),
     Post =>
       Element (Contents, Fd).String
       = Append (Element (Contents_Pcd_Entry, Fd).String,
                 Buf,
                 Has_Written + Has_Written_B);

   procedure Prove_Loop_Invariant_String_Version
     (String, Old_String, Pcd_Entry_String    : Unbounded_String;
      Buf                                     : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S : ssize_t)
   with
     Pre =>
       Num_Bytes_S in 1 .. ssize_t (Integer'Last)
         and then Integer (Num_Bytes_S) <= Buf'Length
         and then Buf'Last < Natural'Last
         and then Has_Written in 0 .. Num_Bytes_S
         and then Has_Written_B in 0 .. Num_Bytes_S - Has_Written
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes_S))'Valid_Scalars
         and then
       Old_String
       = Append (Pcd_Entry_String, Buf, Has_Written)
         and then
       String
       = Append (Old_String,
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B),
     Post =>
       String = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B);

end Lemmas;
