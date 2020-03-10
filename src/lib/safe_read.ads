with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Errors;

use Contents_Table_Type.Maps;

procedure Safe_Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     SPARK_Mode,
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => FD_Table),
     Post =>
     (case Has_Read is
        when -1                =>
          Contents'Old = Contents
            and then Errors.Get_Errno /= Errors.ADA_EINTR,
        when 0                 =>
          Contents'Old = Contents
            and then Has_Key (Contents, Fd),
        when 1 .. ssize_t'Last =>
          Natural (Has_Read) <= Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Initialized
            and then
          Has_Key (Contents, Fd)
            and then
          Same_Keys (Contents'Old, Contents)
            and then
          Is_Append (Get (Contents'Old, Fd), Buf,
                     Get (Contents, Fd), Has_Read)
            and then
          Elements_Equal_Except (Contents'Old, Contents, Fd),
        when others          => False);
