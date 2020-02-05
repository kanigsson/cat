with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Errors;

use Contents_Table_Type.Maps;

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
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars),
  Post =>
    (case Has_Written is
       when -1                =>
         Contents = Contents'Old
           and then Errors.Get_Errno /= Errors.ADA_EINTR,
       when 0                 =>
         Contents = Contents'Old
           and then Has_Key (Contents, Fd),
       when 1 .. ssize_t'Last =>
         size_t (Has_Written) <= Num_Bytes
           and then
         Has_Key (Contents, Fd)
           and then
         Same_Keys (Contents, Contents'Old)
           and then
       Is_Append (Get (Contents'Old, Fd), Buf, Get (Contents, Fd),
                  Has_Written)
           and then
         Elements_Equal_Except (Contents, Contents'Old, Fd),
       when others            => False);
