package body Iostr with
  SPARK_Mode => Off
is

   package body Ghost_Package is

      --  The three functions below are implemented the same way as those in
      --  Ada.Strings.Unbounded.

      -----------------
      --  To_String  --
      -----------------

      function "=" (L, R : Unbounded_String) return Boolean is
        (ASU."=" (L.Str, R.Str));

      --------------
      --  Length  --
      --------------

      function Length (Source : Unbounded_String) return Natural is
        (ASU.Length (Source.Str));

      -----------------
      --  To_String  --
      -----------------

      function To_String (Source : Unbounded_String) return String is
        (ASU.To_String (Source.Str));

      ---------
      --  &  --
      ---------

      function "&" (L, R : Unbounded_String) return Unbounded_String is
         Result : ASU.Unbounded_String := L.Str;
      begin
         if Length (L) = Natural'Last then
            null;
            --  First case: returned Unbounded_String is L

         elsif Length (R) <= Natural'Last - Length (L) then
            ASU.Append (Result, R.Str);
            --  Second case: R is appended entirely

         else
            ASU.Append (Result, ASU.Head (R.Str, Natural'Last - Length (L)));
            --  Third case: Natural'Last - Length (L) first elements from
            --  R are appended to L.
         end if;

         return Unbounded_String'(Str => Result);
      end "&";

      --------------
      --  Append  --
      --------------

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
            --  First case: returned Unbounded_String is L

         elsif Natural (Bytes) <= Natural'Last - Length (L) then
            ASU.Append
              (Result,
               To_String (R (R'First .. R'First - 1 + Natural (Bytes))));
            --  Second case: Bytes characters are appended

         else
            ASU.Append
              (Result,
               To_String (R (R'First
                              ..
                             Natural'Last - Length (L) - 1 + R'First)));
            --  Third case: Natural'Last - Length (L) characters are appended
         end if;

         return Unbounded_String'(Str => Result);
      end Append;

      -----------------
      --  To_String  --
      -----------------

      function To_String (Source : Init_String) return String is
         Result : String (1 .. Source'Length);
      begin
         for J in 1 .. Source'Length loop
            Result (J) := Source (Source'First - 1 + J);
         end loop;
         --  All characters are copied into Result

         return Result;
      end To_String;
   end Ghost_Package;
end Iostr;

