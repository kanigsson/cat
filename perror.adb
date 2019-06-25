with Interfaces.C; use Interfaces.C;

procedure Perror (Str : String) with SPARK_Mode is
  procedure C_Perror (Str : char_array) with Global => Errors.Error_State;
  pragma Import (C, C_Perror, "perror");
begin
   C_Perror (To_C (Str));
end Perror;
