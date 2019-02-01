with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Stdio; use Stdio;
with Errors;

procedure Cat with SPARK_Mode is

   X : int;
   Err : int;
   Buf : Init_String (1 .. 1024);
   Has_Read, Has_Written : ssize_t;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Open (To_C (Ada.Command_Line.Argument (I)), ADA_O_RDONLY, X);
      if X = -1 then
         case Errors.Get_Errno is
            when Errors.ADA_ENOENT =>
               Ada.Text_IO.Put_Line ("file does not exist");
            when others =>
               Ada.Text_IO.Put_Line ("unknown errors");
         end case;
         return;
      end if;
      pragma Assert (X >= 0);
      loop
         Read (X, Buf, Has_Read);
	 exit when Has_Read <= 0;
	 Write (Stdout, Buf, Size_T (Has_Read), Has_Written);
	 pragma Assert (Has_Read = Has_Written);
      end loop;
      Close(X, Err);
   end loop;
end Cat;
