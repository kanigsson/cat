with Ada.Unchecked_Deallocation;
--with Ada.Text_IO; use Ada.Text_IO;
package body Iostr with
  SPARK_Mode => Off
is

   package body Ghost_Package is

      function Length (Source : Unbounded_String) return Natural is
        (ASU.Length (ASU.Unbounded_String (Source)));

      function To_String (Source : Unbounded_String) return String is
        (ASU.To_String (ASU.Unbounded_String (Source)));

      overriding
      function "&" (L, R : Unbounded_String) return Unbounded_String is
         Result : Unbounded_String := L;
      begin
         if Length (L) = Natural'Last or else Length (R) = 0 then
            null;
         elsif Length (R) <= Natural'Last - Length (L) then
            Append (Result, R);
         else
            Append (Result, Head (R, Natural'Last - Length (L)));
         end if;
         return Result;
      end "&";

      function Append
        (L     : Unbounded_String;
         R     : Init_String;
         Bytes : Int)
         return  Unbounded_String
      is
         Result : Unbounded_String := L;
      begin
         if Length (L) = Natural'Last or else Bytes = 0 then
            null;
         elsif Natural (Bytes) <= Natural'Last - Length (L) then
            Append (Result, To_String (R (R'First .. R'First - 1 + Natural (Bytes))));
         else
            Append (Result, To_String (R (R'First .. Natural'Last - Length (L) - 1 + R'First)));
         end if;
         return Result;
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
