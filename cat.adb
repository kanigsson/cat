with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Const_H;

procedure Cat with SPARK_Mode is

   function C_Open (File : char_array; Flags : int) return int;
   pragma Import (C, C_Open, "open");

begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Ada.Text_IO.Put_Line (Ada.Command_Line.Argument (I));
   end loop;

   ADa.Text_IO.Put_Line (unsigned'Image (Const_H.Ada_O_RDONLY));
   ADa.Text_IO.Put_Line (unsigned'Image (Const_H.Ada_O_RDWR));
end Cat;
