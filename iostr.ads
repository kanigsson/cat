with Interfaces.C; use Interfaces.C;
with Init_Strings; use Init_Strings;

package Iostr with
  Ghost,
  SPARK_Mode => On
is

   function To_String (Source : Init_String) return String with
     Pre    => Source'Valid_Scalars,
     Post   =>
       To_String'Result'First = 1
         and then To_String'Result'Last = Source'Length,
     Global => null;

   type Unbounded_String is private with
     Default_Initial_Condition => Length (Unbounded_String) = 0;

   function Length (Source : Unbounded_String) return Natural with Global => null;

   function To_String (Source : Unbounded_String) return String with
     Global => null,
     Post   => To_String'Result'First = 1
                 and then
               To_String'Result'Last = Length (Source);

   function "=" (L, R : Unbounded_String) return Boolean is
     (To_String (L) = To_String (R))
   with
     Global => null;

   function "&" (L, R : Unbounded_String) return Unbounded_String with
     Import,
     Pre  => Length (L) <= Natural'Last - Length (R),
     Post =>
       (if Length (R) = 0
        then To_String ("&"'Result) = To_String (L)
        else To_String ("&"'Result) = To_String (L) & To_String (R));

   function Append
     (Left  : Unbounded_String;
      Right : Init_String;
      Bytes : Int)
      return     Unbounded_String
   with
     Global => null,
     Pre =>
       (if Bytes >= 0
        then Natural (Bytes) <= Right'Length
                and then
          Natural (Bytes) <= Natural'Last - Length (Left)
               and then
             Right (Right'First.. Right'First - 1 + Natural (Bytes))'Valid_Scalars),
     Post =>
       (if Bytes <= 0
        then To_String (Append'Result) = To_String (Left)
        else To_String (Append'Result)
           = To_String (Left)
             & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes))));

   procedure Append_Pcd
     (Left  : in out Unbounded_String;
      Right : Init_String;
      Bytes : Int)
   with
     Global => null,
     Pre =>
         (if Bytes >= 0
          then
            Natural (Bytes) <= Right'Length
              and then
            Natural (Bytes) <= Natural'Last - Length (Left)
              and then
            Right (Right'First.. Right'First - 1 + Natural (Bytes))'Valid_Scalars),
     Post =>
       (if Bytes <= 0
        then To_String (Left) = To_String (Left'Old)
        else To_String (Left)
           = To_String (Left'Old)
             & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes))));

private

   pragma SPARK_Mode (Off);

   type String_Access_Base is access all String;

   subtype String_Access is not null String_Access_Base;

   function Init_Content (L : Natural := 100) return String_Access;

   type Unbounded_String is record
      Length  : Natural := 0;
      Content : String_Access := Init_Content;
   end record;

end Iostr;
