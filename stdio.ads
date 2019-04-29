with Const_H;
with Errors;
with Interfaces.C; use Interfaces.C;

package Stdio with SPARK_Mode,
 Abstract_State => (FD_Table,
                    Contents_State)
is

   subtype ssize_t is int;
   subtype off_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;

   procedure Open (File : char_array; Flags : int; Fd : out Int)
     with Global => (In_Out => (FD_Table,Errors.Error_State)),
          Post =>
     (Fd >= 0 or else Fd = -1);

   procedure Close (Fd : int; Result : out Int)
   with Global => (In_Out => (FD_Table, Errors.Error_State));

   function Has_Reading (Flags : int) return Boolean is
   (Flags mod 4 in Const_H.ADA_O_RDWR | Const_H.ADA_O_RDONLY);

   function Contents (Fd : int) return String
   with Global => (Proof_In => Contents_State);

   function EOF_Contents (Fd : int) return String
   with Global => (Proof_In => Contents_State);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with Global => (In_Out => (Errors.Error_State, Contents_State),
                   Proof_In => (FD_Table, Const_H.ADA_O_RDONLY, Const_H.ADa_O_RDWR)),
        Post =>
     (Has_Read <= Buf'Length and then
	(if Has_Read > 0 then
            (Buf (Buf'First .. Buf'First + Positive (Has_Read) - 1)'Valid_scalars
             and then Contents (Fd) = Contents (Fd)'Old & 
                                      String (Buf (Buf'First .. Buf'First + Positive (Has_Read) - 1)))
          elsif Has_Read = 0 then Contents (Fd) = Contents (Fd)'Old );

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
