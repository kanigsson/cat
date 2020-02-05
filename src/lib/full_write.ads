with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;

use Contents_Table_Type.Maps;

procedure Full_Write
  (Fd        : int;
   Buf       : Init_String;
   Num_Bytes : size_t;
   Err       : out int)
with
  SPARK_Mode => On,
  Pre    =>
    (Num_Bytes <= size_t (Integer'Last)
       and then ssize_t (Num_Bytes) <= Buf'Length
       and then Buf'Last < Natural'Last
       and then Num_Bytes > 0
       and then
     Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars),
  Post =>
    (case Err is
       when -1     =>
         Same_Keys (Contents, Contents'Old)
           and then
         Elements_Equal_Except (Contents, Contents'Old, Fd),
       when 0      =>
         Has_Key (Contents, Fd)
           and then
         Same_Keys (Contents, Contents'Old)
           and then
       Is_Append (Get (Contents'Old, Fd), One_String (Buf),
                  Get (Contents, Fd),
                  ssize_t (Num_Bytes))
           and then
         Elements_Equal_Except (Contents, Contents'Old, Fd),
       when others => False);
