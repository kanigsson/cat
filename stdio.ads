with Const_H;
with Interfaces.C; use Interfaces.C;

package Stdio with SPARK_Mode is

   subtype ssize_t is int;
   subtype off_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;

   function Open (File : char_array; Flags : int) return Int
     with Post => (Open'Result = -1 or else Open'Result >= 0);
   function Close (Fd : int) return Int;

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
     with Post =>
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
