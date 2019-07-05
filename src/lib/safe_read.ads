with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Errors;

use Iostr.Ghost_Package;
use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type.Formal_Maps.Formal_Model;

--  Safe_Write does not set Has_Read to -1 if an ADA_EINTR occurs. Instead,
--  it tries to read until Has_Read >= 0 or (Has_Read = -1 and
--  Get_Errno /= ADA_EINTR).
procedure Safe_Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     SPARK_Mode,
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => FD_Table),
     Post =>
     (case Has_Read is
        when -1                =>
          Contents = Contents'Old
            and then Errors.Get_Errno /= Errors.ADA_EINTR,
        --  An error occured. Nothing has changed in Contents, and the
        --  error is not EINTR.

        when 0                 =>
          Contents = Contents'Old
            and then Contains (Contents, Fd),
        --  End of file has been reached, nothing has been read so Contents
        --  is the same as before.

        when 1 .. ssize_t'Last =>
          Contains (Contents, Fd)
          --  The file is open

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

        when others          => False); --  Unreachable case
