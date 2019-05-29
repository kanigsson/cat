with Interfaces.C; use Interfaces.C;
private with Ada.Strings.Unbounded;

package Iostr with
  SPARK_Mode => On
is
   pragma Annotate (GNATProve, Terminating, Iostr);

   subtype ssize_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;

   package Ghost_Package with Ghost is
      function To_String (Source : Init_String) return String with
        Global => null,
        Pre    => Source'Valid_Scalars,
        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Source'Length;

      type Unbounded_String is private with
        Default_Initial_Condition => Length (Unbounded_String) = 0;

      type Unbounded_String_Holder is record
        String : Unbounded_String;
      end record;

      function Null_Unbounded_String return Unbounded_String with
        Global => null,
        Post   => Length (Null_Unbounded_String'Result) = 0;

      function Null_Unbounded_String_Holder return Unbounded_String_Holder is
        (Unbounded_String_Holder'(String => Null_Unbounded_String))
      with
        Global => null,
        Post   => Length (Null_Unbounded_String_Holder'Result.String) = 0;

      function To_Unbounded_String_Holder
        (Source : Unbounded_String)
         return   Unbounded_String_Holder
      is
        (Unbounded_String_Holder'(String => Source));

      function Length (Source : Unbounded_String) return Natural with
        Global => null;

      function To_String (Source : Unbounded_String) return String with
        Global => null,
        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Length (Source);

      function "=" (L, R : Unbounded_String) return Boolean with
        Global => null,
        Post   => "="'Result = (To_String (L) = To_String (R));

      function "&" (L, R : Unbounded_String) return Unbounded_String with
        Global         => null,
        Contract_Cases =>
          (Length (R) = 0
             or else
           Length (L) = Natural'Last    => To_String ("&"'Result) = To_String (L),
           Length (R) > 0
             and then
           Length (R)
           <= Natural'Last - Length (L) => To_String ("&"'Result)
                                           = To_String (L) & To_String (R),
           others                       => To_String ("&"'Result)
                                           = To_String (L)
                                           & To_String (R)
                                               (1
                                                 ..
                                                Natural'Last - Length (L)));
      function Append
        (L  : Unbounded_String;
         R : Init_String;
         Bytes : Int)
      return     Unbounded_String
        with
          Global       => null,
          Pre          =>
            (if Bytes >= 0
            then
              Natural (Bytes) <= R'Length
                and then
              R (R'First.. R'First - 1 + Natural (Bytes))'Valid_Scalars),
        Contract_Cases =>
          (Bytes <= 0
             or else
           Length (L) = Natural'Last    => To_String (Append'Result) = To_String (L),
           Bytes > 0
             and then
           Natural (Bytes)
           <= Natural'Last - Length (L) => To_String (Append'Result)
                                           = To_String (L)
                                           & To_String
                                               (R (R'First
                                                     ..
                                                   R'First - 1 + Natural (Bytes))),
           others                       => To_String (Append'Result)
                                           = To_String (L)
                                           & To_String
                                               (R (R'First
                                                     ..
                                                   Natural'Last - Length (L) - 1 + R'First)));

   private

      pragma SPARK_Mode (Off);
      package ASU renames Ada.Strings.Unbounded;

      type Unbounded_String is record
        Str : ASU.Unbounded_String;
      end record;

      function Null_Unbounded_String return Unbounded_String is
        (Unbounded_String'(Str => ASU.Null_Unbounded_String));

      end Ghost_Package;
end Iostr;
