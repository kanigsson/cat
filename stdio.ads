with Const_H;
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
   with Ghost,
        Global => (FD_Table);
   --  return True Fd has an entry in the FD table

   function FD_Flags (Fd : Int) return Int
   with Ghost,
   Pre => Valid_Fd (Fd);
   --  return the flags which which the FD was opened

   procedure Open (File : char_array; Flags : int; Fd : out Int)
     with Global => (In_Out => FD_Table),
          Post =>
     ((if Fd >= 0 then
         Valid_Fd (Fd) and then
         FD_Flags (Fd) = Flags
       else Fd = -1));

   procedure Close (Fd : int; Result : out Int)
   with Global => (In_Out => FD_Table),
        Pre    => Valid_Fd (Fd);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
     with Pre =>
     (Valid_Fd (Fd)),
          Post =>
     (Has_Read <= Buf'Length and then
	(if Has_Read > 0 then Buf (Buf'First .. Buf'First + Positive (Has_Read) - 1)'Valid_scalars));

  procedure Write (Fd : int; Buf : in Init_String; Num_Bytes : Size_T; Has_Written : out ssize_t)
    with Pre =>
    (Num_Bytes <= Buf'Length and then
     Num_Bytes > 0 and then
       Buf (Buf'First .. Buf'First + Positive (Num_Bytes) - 1)'Valid_scalars);

   Stdout : int := 1;

   ADA_O_RDONLY : Int := Const_H.ADA_O_RDONLY;
   ADA_O_RDWR   : Int := Const_H.ADA_O_RDWR;
end Stdio;
