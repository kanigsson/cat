with Interfaces.C; use Interfaces.C;
--with Init_Strings; use Init_Strings;

package Iostr with
  SPARK_Mode => On
is

   subtype ssize_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;

   package Ghost_Package with Ghost is
      function To_String (Source : Init_String) return String with
        Pre    => Source'Valid_Scalars,
        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Source'Length,
        Global => null;

      type Unbounded_String is private with
        Default_Initial_Condition => Length (Unbounded_String) = 0;

      function Empty_Unbounded_String return Unbounded_String with
        Post => Length (Empty_Unbounded_String'Result) = 0;

      function Length (Source : Unbounded_String) return Natural with Global => null;

      function To_String (Source : Unbounded_String) return String with
        Global => null,
        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Length (Source);

      procedure Reset (Source : in out Unbounded_String) with
        Import,
        Post => Length (Source) = 0;

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

      procedure U_Append_Def (L, R : Unbounded_String) with
        Pre  => Length (L) <= Natural'Last - Length (R),
        Post =>
          (if Length (R) = 0
                 then To_String (L & R) = To_String (L)
                 else To_String (L & R) = To_String (L) & To_String (R));
      procedure U_Append_Def (L, R : Unbounded_String) is null;

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

--        procedure Append_Pcd
--          (Left  : in out Unbounded_String;
--           Right : Init_String;
--           Bytes : Int)
--          with
--            Global => null,
--            Pre =>
--              (if Bytes >= 0
--                 then
--                   Natural (Bytes) <= Right'Length
--               and then
--                 Natural (Bytes) <= Natural'Last - Length (Left)
--               and then
--                 Right (Right'First.. Right'First - 1 + Natural (Bytes))'Valid_Scalars),
--          Post =>
--            (if Bytes <= 0
--               then To_String (Left) = To_String (Left'Old)
--                   else To_String (Left)
--             = To_String (Left'Old)
--             & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes))));

      procedure Append_Def
        (Left  : Unbounded_String;
         Right : Init_String;
         Bytes : Int)
        with
          Ghost,
          Pre =>
            Bytes > 0
            and then
              Natural (Bytes) <= Right'Length
        and then
          Natural (Bytes) <= NAtural'LAst - Length (Left)
        and then
          Right (Right'First.. Right'First - 1 + Natural (Bytes))'Valid_Scalars,
          Post =>
            To_String (Append (Left, Right, Bytes))
              = To_String (Left)
        & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes)));

      procedure Append_Def
        (Left  : Unbounded_String;
         Right : Init_String;
         Bytes : Int)
      is null;

      procedure String_Append_Equal (A, B, C, D : String) with
        Pre  =>
          A = B
          and then C = D
          and then A'First = 1
          and then B'First = 1
          and then C'Length <= Natural'Last - A'Length,
          Post => A & C = B & D;
      procedure String_Append_Equal (A, B, C, D : String) is null;

   private

      pragma SPARK_Mode (Off);

         type String_Access_Base is access all String;

         subtype String_Access is not null String_Access_Base;

         function Init_Content (L : Natural := 100) return String_Access;

         type Unbounded_String is record
            Length  : Natural := 0;
            Content : String_Access := Init_Content;
         end record;

         function Empty_Unbounded_String return Unbounded_String is
           (Unbounded_String'(Length => 0, Content => Init_Content));

   end Ghost_Package;
end Iostr;
