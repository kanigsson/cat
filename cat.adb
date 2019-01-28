with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Const_H;
with System;

procedure Cat with SPARK_Mode is

   function C_Open (File : char_array; Flags : int) return int;
   pragma Import (C, C_Open, "open");

   subtype size_t is unsigned;

   subtype ssize_t is int;
   subtype off_t is int;

   function C_Read (Fd : int; Buf : System.Address; Size : size_t; Offset : off_t)
                    return ssize_t;
   pragma Import (C, C_Read, "read");

   function C_Close (Fd : int) return int;
   pragma Import (C, C_Close, "close");


   procedure Read (Fd : int; Buf : in out String; Has_Read : out ssize_t) is
   begin
      Has_Read := C_Read (Fd, Buf'Address, Buf'Length, 0);
   end Read;

   X : int;
   Err : int;
   Buf : String (1 .. 1024);
   Has_Read : ssize_t;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      X := C_Open (To_C (Ada.Command_Line.Argument (I)), const_h.ADA_O_RDONLY);
      pragma Assert (X >= 0);
      Read (X, Buf, Has_Read);
      Err := C_Close(X);
      pragma Assert (X >= 0);
      Ada.Text_IO.Put_Line (Buf (Integer(1) .. Integer(Has_Read)));
   end loop;
end Cat;
