with Errors;

procedure Perror (Str : String) with SPARK_Mode, Global => Errors.Error_State;
