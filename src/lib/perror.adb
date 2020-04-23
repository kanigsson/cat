with Interfaces.C; use Interfaces.C;

procedure Perror (Str : String) with SPARK_Mode is

   --  Binding to C function
   procedure C_Perror (Str : char_array) with Global => Errors.Error_State;
   pragma Import (C, C_Perror, "perror");

begin
   C_Perror (To_C (Str));
end Perror;
