with System; --with Ada.Text_IO; use Ada.Text_IO;

package body Stdio with SPARK_Mode => Off
is

   function C_Open (File : char_array; Flags : int) return int;
   pragma Import (C, C_Open, "open");

   function C_Read (Fd : int; Buf : System.Address; Size : size_t; Offset : off_t)
                    return ssize_t;
   pragma Import (C, C_Read, "read");

   function C_Close (Fd : int) return int;
   pragma Import (C, C_Close, "close");

   function C_Write (Fd : int; Buf : System.Address; Nbyte : size_t)
                    return ssize_t;
   pragma Import (C, C_Write, "write");

   procedure Open (File : char_array; Flags : int; Fd : out Int) is
   begin
      Fd := C_Open (File, Flags);
   end Open;

   procedure Close (Fd : int; Result : out Int) is
   begin
      Result := C_Close (Fd);
   end Close;

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t) is
   begin
      Has_Read := C_Read (Fd, Buf'Address, Buf'Length, 0);
      Append_Pcd (Contents (Fd), Buf, Has_Read);
   end Read;

   procedure Write (Fd : int; Buf : in Init_String; Num_Bytes : Size_T; Has_Written : out ssize_t) is
   begin
      Has_Written := C_Write (Fd, Buf'Address, Num_Bytes);
      Append_Pcd (Contents (Fd), Buf, Has_Written);
   end Write;

end Stdio;
