with Const_H;
with Interfaces.C; use Interfaces.C;

package Stdio is

   subtype ssize_t is int;
   subtype off_t is int;

   function Open (File : char_array; Flags : int) return Int;
   function Close (Fd : int) return Int;

   procedure Read (Fd : int; Buf : in out String; Has_Read : out ssize_t);
   procedure Write (Fd : int; Buf : in out String; Num_Bytes : Size_T; Has_Written : out ssize_t);

   Stdout : int := 1;

   ADA_O_RDONLY : Int := Const_H.ADA_O_RDONLY;
   ADA_O_RDWR   : Int := Const_H.ADA_O_RDWR;
end Stdio;
