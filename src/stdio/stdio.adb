with System;

package body Stdio with
  SPARK_Mode => Off
is
   function C_Open (File : char_array; Flags : int) return int;
   pragma Import (C, C_Open, "open");

   function C_Read
     (Fd     : int;
      Buf    : System.Address;
      Size   : size_t;
      Offset : off_t) return ssize_t;
   pragma Import (C, C_Read, "read");

   function C_Close (Fd : int) return int;
   pragma Import (C, C_Close, "close");

   function C_Write (Fd : int; Buf : System.Address; Nbyte : size_t)
                    return ssize_t;
   pragma Import (C, C_Write, "write");

   procedure Open (File : char_array; Flags : int; Fd : out int) is
   begin
      Fd := C_Open (File, Flags);
      if Fd >= 0 then
         Contents := Add (Contents, Fd, "");
      end if;
   end Open;

   procedure Close (Fd : int; Result : out int) is
   begin
      Result := C_Close (Fd);
      if Result = 0 then
         Contents := Remove (Contents, Fd);
      end if;
   end Close;

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t) is
   begin
      Has_Read := C_Read (Fd, Buf'Address, Buf'Length, 0);
      if Has_Read > 0 then
         Contents := Add (Contents, Fd,
                          Append (Get (Contents, Fd), One_String (Buf),
                                  Has_Read));
      end if;
   end Read;

   procedure Write
     (Fd          : int;
      Buf         : in Init_String;
      Num_Bytes   : size_t;
      Has_Written : out ssize_t) is
   begin
      Has_Written := C_Write (Fd, Buf'Address, Num_Bytes);
      if Has_Written > 0 then
         Contents :=
           Add (Contents, Fd,
                Append (Get (Contents, Fd), One_String (Buf), Has_Written));
      end if;
   end Write;

   procedure Reset (Fd : int) is
   begin
      if Has_Key (Contents, Fd) then
         Contents := Add (Contents, Fd, "");
         null;
      end if;
   end Reset;

begin
   Contents := Add (Contents, Stdin,  "");
   Contents := Add (Contents, Stdout, "");
   Contents := Add (Contents, Stderr, "");
end Stdio;
