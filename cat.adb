with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Stdio; use Stdio;

procedure Cat with SPARK_Mode is

   X : int;
   Err : int;
   Buf : String (1 .. 1024);
   Has_Read, Has_Written : ssize_t;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      X := Open (To_C (Ada.Command_Line.Argument (I)), ADA_O_RDONLY);
      pragma Assert (X >= 0);
      loop
         Read (X, Buf, Has_Read);
	 exit when Has_Read <= 0;
	 Write (Stdout, Buf, Size_T (Has_Read), Has_Written);
	 pragma Assert (Has_Read = Has_Written);
      end loop;
      Err := Close(X);
   end loop;
end Cat;
