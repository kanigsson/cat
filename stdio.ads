with Const_H;      use Const_H;
with Errors;
with Iostr;        use Iostr;
with Interfaces.C; use Interfaces.C;
with Init_Strings; use Init_Strings;

package Stdio with
  SPARK_Mode,
  Abstract_State => (FD_Table),
  Initializes => (FD_Table,
                  Contents)
is

   type Contents_Table_Type is array (int range 0 .. 1023) of Unbounded_String with Ghost;
   
   Contents : Contents_Table_Type with Ghost;
   
   subtype off_t is int;

   procedure Open (File : char_array; Flags : int; Fd : out Int) with
     Global => (In_Out => (FD_Table,Errors.Error_State)),
     Post => (Fd in 3 .. 1023 or else Fd = -1);

   procedure Close (Fd : int; Result : out Int) with
     Global => (In_Out => (FD_Table, Errors.Error_State));

   function Has_Reading (Flags : int) return Boolean is
     (Flags mod 4 in Const_H.ADA_O_RDWR | Const_H.ADA_O_RDONLY);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => (FD_Table, Const_H.ADA_O_RDONLY, Const_H.ADa_O_RDWR)),
     Post =>
       (Has_Read <= Buf'Length
       and then
          (for all J in int range 0 .. 1023 =>
             (if J /= Fd then Contents (J) = Contents'Old (J)))
        and then
          (if Has_Read > 0
           then (Buf (Buf'First .. Buf'First + Positive (Has_Read) - 1)'Valid_scalars
                   and then Contents (Fd) = Append (Contents'Old (Fd), Buf, Has_Read))
           elsif Has_Read <= 0
             then Contents (Fd) = Contents'Old (Fd)));

   procedure Write
     (Fd          : int;
      Buf         : in Init_String;
      Num_Bytes   : Size_T;
      Has_Written : out ssize_t)
   with
     Global => (In_Out => (Errors.Error_State, Contents)),
     Pre    =>
       (Num_Bytes <= Buf'Length
         and then Num_Bytes > 0
         and then Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_scalars),
     Post   =>
       Num_Bytes = Size_T (Has_Written)
     and then 
       Contents (Fd) = Append (Contents'Old (Fd), Buf, Has_Written)
     and then
       (for all J in int range 0 .. 1023 =>
         (if J /= Fd then Contents (J) = Contents'Old (J)));

   Stdin  : constant int := 0;
   Stdout : constant int := 1;
   Stderr : constant int := 2;

end Stdio;
