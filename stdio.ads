with Const_H;
with Errors;
with Interfaces.C; use Interfaces.C;

package Stdio with SPARK_Mode,
 Abstract_State => (FD_Table)
is

   subtype ssize_t is int;
   subtype off_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;


   function Valid_Fd (Fd : Int) return Boolean
   with
   --Ghost,
        Global => (Input => FD_Table);
   --  return True Fd has an entry in the FD table

   function FD_Flags (Fd : Int) return Int
   with 
   --Ghost,
        Global => (Input => FD_Table),
        Pre => Valid_Fd (Fd);
   --  return the flags which which the FD was opened

   procedure Open (File : char_array; Flags : int; Fd : out Int)
     with Global => (In_Out => (FD_Table,Errors.Error_State)),
          Post =>
     ((if Fd >= 0 then
         Valid_Fd (Fd) and then
         FD_Flags (Fd) = Flags
       else Fd = -1));

   procedure Close (Fd : int; Result : out Int)
   with Global => (In_Out => (FD_Table, Errors.Error_State)),
        Pre    => Valid_Fd (Fd);

   function Has_Reading (Flags : int) return Boolean is
   (Flags mod 4 in Const_H.ADA_O_RDWR | Const_H.ADA_O_RDONLY);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with Global => (In_Out => Errors.Error_State,
                   Proof_In => (FD_Table, Const_H.ADA_O_RDONLY, Const_H.ADa_O_RDWR)),
        Pre =>
     (Valid_Fd (Fd) and then Has_Reading (FD_Flags (FD))),
        Post =>
     (Has_Read <= Buf'Length and then
	(if Has_Read > 0 then Buf (Buf'First .. Buf'First + Positive (Has_Read) - 1)'Valid_scalars));

  procedure Write (Fd : int; Buf : in Init_String; Num_Bytes : Size_T; Has_Written : out ssize_t)
   with Global => (In_Out => Errors.Error_State),
        Pre =>
    (Num_Bytes <= Buf'Length and then
     Num_Bytes > 0 and then
       Buf (Buf'First .. Buf'First + Positive (Num_Bytes) - 1)'Valid_scalars);

   Stdin : int := 0;
   Stdout : int := 1;
   Stderr : int := 2;

   ADA_O_RDONLY : Int := Const_H.ADA_O_RDONLY;
   ADA_O_RDWR   : Int := Const_H.ADA_O_RDWR;
end Stdio;
