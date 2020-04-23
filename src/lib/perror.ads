with Errors;

--  Perror is a binding to the C function. It prints the official error
--  messages.
procedure Perror (Str : String) with SPARK_Mode, Global => Errors.Error_State;
