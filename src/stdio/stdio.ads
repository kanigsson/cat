with Ada.Containers;      use Ada.Containers;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Errors;
use Contents_Table_Type.Maps;

package Stdio with
  SPARK_Mode        => On,
  Abstract_State    => (FD_Table),
  Initializes       => (FD_Table,
                        Contents),
  Initial_Condition =>
    (Has_Key (Contents, Stdin)
       and then Has_Key (Contents, Stdout)
       and then Has_Key (Contents, Stderr)
       and then One_String'(Get (Contents, Stdin))'Length = 0
       and then One_String'(Get (Contents, Stdout))'Length = 0
       and then One_String'(Get (Contents, Stderr))'Length = 0)
is
   pragma Elaborate_Body;

   OPEN_MAX : constant Count_Type := 1024;
   --  Hard-coded constant, maximum number of files that can be open
   --  at the same time.

   Contents : Map with Ghost;
   --  This variable maps FD to the content of the corresponding file

   subtype off_t is int;

   procedure Open (File : char_array; Flags : int; Fd : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
       (case Fd is
          when -1                      => Contents'Old = Contents,
          --  An error occured, nothing has changed in Contents
          when 0 .. int (OPEN_MAX - 1) =>
            Length (Contents'Old) + 1 = Length (Contents)
              and then
            Has_Key (Contents, Fd)
              and then
            One_String'(Get (Contents, Fd))'Length = 0
            --  The unbounded string associated with Fd is empty for now
              and then
            not Has_Key (Contents'Old, Fd)
              and then
            Contents'Old <= Contents
              and then
            Keys_Included_Except (Contents,
                                    Contents'Old,
                                    Fd),
           --  Contents is the same as before, except that it has a new key
           --  that wasn't present before: Fd.
          when others                  => False); --  Unreachable case
   procedure Close (Fd : int; Result : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
     (case Result is
       when -1     => Contents = Contents'Old,
       --  An error occured, nothing has changed in Contents.

       when 0      =>
          Length (Contents) = Length (Contents'Old) - 1
            and then
          not Has_Key (Contents, Fd)
            and then
        Contents <= Contents'Old
            and then
          Keys_Included_Except (Contents'Old, Contents, Fd),
          --  Contents is the same as before, except that Fd has been removed.
       when others => False); --  Unreachable case

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => (FD_Table)),
     Post =>
     (case Has_Read is
        when -1                =>
          Contents = Contents'Old
            and then (if Errors.Get_Errno = Errors.ADA_EINTR
                      then Has_Key (Contents, Fd)),
        --  An error occured. Nothing has changed in Contents, and if the error
        --  is EINTR, that means that Fd is open but it was not possible to
        --  read data from it because of an interrupt.
        when 0                 =>
          Contents = Contents'Old
            and then Has_Key (Contents, Fd),
        --  End of file has been reached, nothing has been read so Contents
        --  is the same as before.
        when 1 .. ssize_t'Last =>
          Has_Key (Contents, Fd)
          --  The file is open (because it is in the FD table)

            and then
          Natural (Has_Read) <= Buf'Length
          --  We read less characters than those available in the buffer

            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Initialized
          --  The first Has_Read characters have been initialized
            and then
          Has_Key (Contents, Fd)
            and then
          Same_Keys (Contents, Contents'Old)
            and then
        Is_Append (Get (Contents'Old, Fd), Buf,
                   Get (Contents, Fd), Has_Read)
          --  This content has been changed to its value before the call
          --  appended to the string we read.
            and then
          Elements_Equal_Except (Contents, Contents'Old, Fd),
          --  Contents is the same as before except for the content in Fd
        when others          => False);  --  Unreachable case

   procedure Write
     (Fd          : int;
      Buf         : in Init_String;
      Num_Bytes   : size_t;
      Has_Written : out ssize_t)
   with
     Global => (In_Out => (Errors.Error_State, Contents)),
     Pre    =>
       (Num_Bytes <= size_t (Integer'Last)
          and then Integer (Num_Bytes) <= Buf'Length
          and then Num_Bytes > 0
          --  It is necessary to write less characters than those available
          --  in the buffer, but more than 1.

          and then
        Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Initialized),
        --  The first Num_Bytes characters of the buffer have to be
        --  initialized.
     Post =>
       (case Has_Written is
          when -1                =>
            Contents = Contents'Old
              and then (if Errors.Get_Errno = Errors.ADA_EINTR
                        then Has_Key (Contents, Fd)),
          --  An error occured. Nothing has changed in Contents, and if the
          --  error is EINTR, that means that Fd is open but it was not
          --  possible to read data from it because of an interrupt.
          when 0                 =>
            Contents = Contents'Old
              and then Has_Key (Contents, Fd),
          --  If nothing was written, this means that the file is open and
          --  Contents is the same as before.
          when 1 .. ssize_t'Last =>
            size_t (Has_Written) <= Num_Bytes
            --  It is possible that less characters than expected have been
            --  written.
              and then
          Has_Key (Contents, Fd)
            --  The file is open
              and then
            Same_Keys (Contents, Contents'Old)
              and then
          Is_Append (Get (Contents'Old, Fd),
                     Buf,
                     Get (Contents, Fd),
                       Has_Written)
          --  This content has been changed to its old value appended to
          --  the string we wrote.
              and then
            Elements_Equal_Except (Contents,
                                     Contents'Old,
                                     Fd),
            --  Contents is the same as before except for the content in Fd
          when others            => False); --  Unreachable case

   --  The Reset procedure is a Ghost procedure which allows to "reset" the
   --  content associated to a file descriptor, without closing and opening it.
   procedure Reset (Fd : int) with
     Ghost,
     Global => (In_Out => Contents),
     Post   =>
       (if not Has_Key (Contents'Old, Fd)
          then Contents'Old = Contents
        --  If it didn't contain the Fd before, Contents is the same as before.

        else
          Same_Keys (Contents, Contents'Old)
            and then
          Elements_Equal_Except (Contents, Contents'Old, Fd)
          --  Else, Contents is the same as before except for the content of Fd
            and then
          One_String'(Get (Contents, Fd))'Length = 0);
          --  ... which is now an empty string


   Stdin  : constant int := 0;
   Stdout : constant int := 1;
   Stderr : constant int := 2;
   --  Hard-coded constants that correspond to Stdin, Stdout and Stderr
   --  in system calls.

end Stdio;
