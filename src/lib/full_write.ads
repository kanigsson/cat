with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;

use Contents_Table_Type.Maps;

--  Since Write may not write the number of Characters in entry, we define
--  Full_Write, that will loop until all characters are written.
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
          --  It is necessary to write less characters than those available
          --  in the buffer, but more than 1.
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Initialized),
     --  The first Num_Bytes characters of the buffer have to be
     --  initialized.
  Post =>
    (case Err is
       when -1     =>
         Same_Keys (Contents'Old, Contents)
           and then
         Elements_Equal_Except (Contents'Old, Contents, Fd),
       --  An error occured while writing. Nothing has changed in Contents
       --  except for the content in Fd. We have no property on this content.
       when 0      =>
         Has_Key (Contents, Fd)
         --  The file is open
           and then
         Same_Keys (Contents'Old, Contents)
           and then
       Is_Append (Get (Contents'Old, Fd), Buf,
                  Get (Contents, Fd),
                  ssize_t (Num_Bytes))
         --  Num_Bytes characters have been written
           and then
         Elements_Equal_Except (Contents'Old, Contents, Fd),
         --  Contents is the same as before, except for the content in Fd
       when others => False); --  Unreachable case
