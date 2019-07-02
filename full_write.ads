with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;

use Iostr.Ghost_Package;
use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type.Formal_Maps.Formal_Model;

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
         M.Same_Keys (Model (Contents), Model (Contents'Old))
           and then
         M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd),
       when 0      =>
         Contains (Contents, Fd)
           and then
         M.Same_Keys (Model (Contents), Model (Contents'Old))
           and then
         Element (Contents, Fd).String
         = Append (Element (Contents'Old, Fd).String, Buf, ssize_t (Num_Bytes))
           and then
         M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd),
       when others => False);
