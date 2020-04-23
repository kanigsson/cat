with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Errors;

use Contents_Table_Type.Maps;

--  Safe_Write does not set Has_Written to -1 if an ADA_EINTR occurs. Instead,
--  it tries to write until Has_Written >= 0 or (Has_Written = -1
--  and Get_Errno /= ADA_EINTR).
procedure Safe_Write
  (Fd          : int;
   Buf         : Init_String;
   Num_Bytes   : size_t;
   Has_Written : out ssize_t)
with
  SPARK_Mode,
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
           and then Errors.Get_Errno /= Errors.ADA_EINTR,
       --  An error occured. Nothing has changed in Contents, and the
       --  error is not EINTR.

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
         Same_Keys (Contents'Old, Contents)
           and then
       Is_Append (Get (Contents'Old, Fd), Buf, Get (Contents, Fd),
                  Has_Written)
         --  This content has been changed to its old value appended to
         --  the string we wrote.
           and then
         Elements_Equal_Except (Contents'Old, Contents, Fd),
         --  Contents is the same as before except for the content in Fd
       when others            => False); --  Unreachable case
