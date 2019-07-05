with Ada.Containers;      use Ada.Containers;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Errors;

use Iostr.Ghost_Package;
use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type.Formal_Maps.Formal_Model;
package Stdio with
  SPARK_Mode        => On,
  Abstract_State    => (FD_Table),
  Initializes       => (FD_Table,
                        Contents),
  Initial_Condition =>
    (Contains (Contents, Stdin)
       and then Contains (Contents, Stdout)
       and then Contains (Contents, Stderr)
       and then Length (Element (Contents, Stdin)) = 0
       and then Length (Element (Contents, Stdout)) = 0
       and then Length (Element (Contents, Stderr)) = 0)
is
   pragma Elaborate_Body;

   OPEN_MAX : constant Count_Type := 1024;
   --  Hard-coded constant, maximum number of files that can be open
   --  at the same time.

   Contents : Map (OPEN_MAX - 1, Default_Modulus (OPEN_MAX - 1)) with Ghost;
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
            Contains (Contents, Fd)
              and then
            not Contains (Contents'Old, Fd)
              and then
            Model (Contents'Old) <= Model (Contents)
              and then
            M.Keys_Included_Except (Model (Contents),
                                    Model (Contents'Old),
                                    Fd)
           --  Contents is the same as before, except that it has a new key
           --  that wasn't present before: Fd.

              and then
            Length (Element (Contents, Fd)) = 0,
            --  The unbounded string associated with Fd is empty for now

          when others                  => False);  --  Unreachable case

   procedure Close (Fd : int; Result : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
     (case Result is
       when -1     => Contents = Contents'Old,
       --  An error occured, nothing has changed in Contents.

       when 0      =>
          Length (Contents) = Length (Contents'Old) - 1
            and then
          not Contains (Contents, Fd)
            and then
          Model (Contents) <= Model (Contents'Old)
            and then
          M.Keys_Included_Except (Model (Contents'Old), Model (Contents), Fd),
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
                      then Contains (Contents, Fd)),
        --  An error occured. Nothing has changed in Contents, and if the error
        --  is EINTR, that means that Fd is open but it was not possible to
        --  read data from it because of an interrupt.

        when 0                 =>
          Contents = Contents'Old
            and then Contains (Contents, Fd),
        --  End of file has been reached, nothing has been read so Contents
        --  is the same as before.

        when 1 .. ssize_t'Last =>
          Contains (Contents, Fd)
          --  The file is open (because it is in the FD table)

            and then
          Natural (Has_Read) <= Buf'Length
          --  We read less characters than those available in the buffer

            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Valid_Scalars
          --  The first Has_Read characters have been initialized

            and then
          M.Same_Keys (Model (Contents), Model (Contents'Old))
            and then
          M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd)
          --  Contents is the same as before except for the content in Fd

            and then
          Element (Contents, Fd)
          = Append (Element (Contents'Old, Fd), Buf, Has_Read),
          --  This content has been changed to its value before the call
          --  appended to the string we read.

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
        Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars),
        --  The first Num_Bytes characters of the buffer have to be
        --  initialized.

     Post =>
       (case Has_Written is
          when -1                =>
            Contents = Contents'Old
              and then (if Errors.Get_Errno = Errors.ADA_EINTR
                        then Contains (Contents, Fd)),
          --  An error occured. Nothing has changed in Contents, and if the
          --  error is EINTR, that means that Fd is open but it was not
          --  possible to read data from it because of an interrupt.

          when 0                 =>
            Contents = Contents'Old
              and then Contains (Contents, Fd),
          --  If nothing was written, this means that the file is open and
          --  Contents is the same as before.

          when 1 .. ssize_t'Last =>
            Contains (Contents, Fd)
            --  The file is open

              and then
            size_t (Has_Written) <= Num_Bytes
            --  It is possible that less characters than expected have been
            --  written.

              and then
            M.Same_Keys (Model (Contents), Model (Contents'Old))
              and then
            M.Elements_Equal_Except (Model (Contents),
                                     Model (Contents'Old),
                                     Fd)
            --  Contents is the same as before except for the content in Fd

              and then
            Element (Contents, Fd)
            = Append (Element (Contents'Old, Fd), Buf, Has_Written),
          --  This content has been changed to its old value appended to
          --  the string we wrote.

          when others            => False); --  Unreachable case

   --  The Reset procedure is a Ghost procedure which allows to "reset" the
   --  content associated to a file descriptor, without closing and opening it.
   procedure Reset (Fd : int) with
     Ghost,
     Global => (In_Out => Contents),
     Post   =>
       (if not Contains (Contents'Old, Fd)
          then Contents'Old = Contents
        --  If it didn't contain the Fd before, Contents is the same as before.

        else
          M.Same_Keys (Model (Contents), Model (Contents'Old))
            and then
          M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd)
          --  Else, Contents is the same as before except for the content of Fd
            and then
          Length (Element (Contents, Fd)) = 0)
          --  ... which is now an empty unbounded string
          ;


   Stdin  : constant int := 0;
   Stdout : constant int := 1;
   Stderr : constant int := 2;
   --  Hard-coded constants that correspond to Stdin, Stdout and Stderr
   --  in system calls.

end Stdio;
