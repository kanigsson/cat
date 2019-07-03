package body Iostr with
  SPARK_Mode => Off
is

   package body Ghost_Package is

      function Length (Source : Unbounded_String) return Natural is
        (ASU.Length (Source.Str));

      function To_String (Source : Unbounded_String) return String is
        (ASU.To_String (Source.Str));

      function "=" (L, R : Unbounded_String) return Boolean is
         use ASU;
      begin
         return L.Str = R.Str;
      end "=";

      function "&" (L, R : Unbounded_String) return Unbounded_String is
         Result : ASU.Unbounded_String := L.Str;
      begin
         if Length (L) = Natural'Last or else Length (R) = 0 then
            null;
         elsif Length (R) <= Natural'Last - Length (L) then
            ASU.Append (Result, R.Str);
         else
            ASU.Append (Result, ASU.Head (R.Str, Natural'Last - Length (L)));
         end if;
         return Unbounded_String'(Str => Result);
      end "&";

      function Append
        (L     : Unbounded_String;
         R     : Init_String;
         Bytes : int)
         return  Unbounded_String
      is
         Result : ASU.Unbounded_String := L.Str;
      begin
         if Length (L) = Natural'Last or else Bytes <= 0 then
            null;
         elsif Natural (Bytes) <= Natural'Last - Length (L) then
            ASU.Append
              (Result,
               To_String (R (R'First .. R'First - 1 + Natural (Bytes))));
         else
            ASU.Append
              (Result,
               To_String (R (R'First
                              ..
                             Natural'Last - Length (L) - 1 + R'First)));
         end if;
         return Unbounded_String'(Str => Result);
      end Append;

      function To_String (Source : Init_String) return String is
         Result : String (1 .. Source'Length);
      begin
         for J in 1 .. Source'Length loop
            Result (J) := Source (Source'First - 1 + J);
         end loop;
         return Result;
      end To_String;

   end Ghost_Package;
end Iostr;

