with System;

package body Stdio with SPARK_Mode => Off is

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

   function Valid_Fd (Fd : Int) return Boolean is
   begin
      -- ??? This function modifies Errors.Errors_State, but we cannot mention
      -- this because this function is supposed to be used in specs/contracts.
      return Const_H.fcntl(fd, Const_H.ADA_F_GETFD) /= -1
      and then Errors.Get_Errno /= Errors.ADA_EBADF;
   end Valid_Fd;

   function FD_Flags (Fd : Int) return Int is
   begin
       -- ??? This function modifies Errors.Errors_State, but we cannot mention
       -- this because this function is supposed to be used in specs/contracts.
       -- ??? why is this GETFD instead of GETFL?
      return Const_H.fcntl(fd, Const_H.ADA_F_GETFD);
   end FD_Flags;

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
   end Read;

   procedure Write (Fd : int; Buf : in Init_String; Num_Bytes : Size_T; Has_Written : out ssize_t) is
   begin
      Has_Written := C_Write (Fd, Buf'Address, Num_Bytes);
   end Write;

end Stdio;
