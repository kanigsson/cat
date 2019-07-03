with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Errors;

use Iostr.Ghost_Package;
use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type.Formal_Maps.Formal_Model;

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
        when 0                 =>
          Contents = Contents'Old
            and then Contains (Contents, Fd),
        when 1 .. ssize_t'Last =>
          Natural (Has_Read) <= Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Valid_Scalars
            and then
          Contains (Contents, Fd)
            and then
          M.Same_Keys (Model (Contents), Model (Contents'Old))
            and then
          Element (Contents, Fd)
          = Append (Element (Contents'Old, Fd), Buf, Has_Read)
            and then
          M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd),
        when others          => False);
