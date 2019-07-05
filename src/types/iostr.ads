with Interfaces.C; use Interfaces.C;
private with Ada.Strings.Unbounded;

package Iostr with
  SPARK_Mode => On
is
   pragma Annotate (GNATProve, Terminating, Iostr);

   subtype ssize_t is int;

   subtype Init_Char is Character;
   pragma Annotate (GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;
   --  Init_Char and Init_String variables will require special attention with
   --  regard to initialization.

   package Ghost_Package with Ghost is
      function To_String (Source : Init_String) return String with
        Global => null,
        Pre    => Source'Valid_Scalars,
        --  We can only do a conversion on a fully initialized Init_String.
        --  It is possible to call this function on a slice of the Init_String
        --  that is initialized.

        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Source'Length
            and then
         (for all J in 1 .. Source'Length =>
            Source (Source'First - 1 + J) = To_String'Result (J))
        --  Init_String and the result string are the same.
        ;

      type Unbounded_String is private with
        Default_Initial_Condition => Length (Unbounded_String) = 0;
      --  This type will be used to represent the content of files.

      --  Null_Unbounded_String returns a empty unbounded string.
      function Null_Unbounded_String return Unbounded_String with
        Global => null,
        Post   => Length (Null_Unbounded_String'Result) = 0;

      --  Length returns the length of an unbounded string.
      function Length (Source : Unbounded_String) return Natural with
        Global => null;

      --  To_String converts an unbounded string to a string.
      function To_String (Source : Unbounded_String) return String with
        Global => null,
        Post   =>
          To_String'Result'First = 1
            and then To_String'Result'Last = Length (Source);

      --  Equality function on two unbounded strings.
      function "=" (L, R : Unbounded_String) return Boolean with
        Global => null,
        Post   => "="'Result = (To_String (L) = To_String (R));

      --  Appends two unbounded strings with no precondition. Different
      --  cases can happen.
      function "&" (L, R : Unbounded_String) return Unbounded_String with
        Global         => null,
        Contract_Cases =>
          (Length (L) = Natural'Last    =>
             To_String ("&"'Result) = To_String (L),
           --  If the Left argument already has maximal length,
           --  then the function returns Left.

           Length (L) < Natural'Last
             and then
           Length (R)
           <= Natural'Last - Length (L) =>
             To_String ("&"'Result) = To_String (L) & To_String (R),
           --  Else, if the Right argument can be appended entirely,
           --  the function returns the two strings appended.

           others                       =>
             To_String ("&"'Result) = To_String (L)
                                      & To_String (R)
                                          (1 .. Natural'Last - Length (L)))
           --  Else, the Right argument cannot be appended entirely, so we
           --  drop the characters that would make the length of the resulting
           --  unbounded string overflow.
           ;

      --  Appends an unbounded string and the first N bytes of an Init_String.
      --  There are no preconditions on the length of the two strings, so
      --  different cases can happen.
      function Append
        (L     : Unbounded_String;
         R     : Init_String;
         Bytes : int)
      return     Unbounded_String
        with
          Global       => null,
          Pre          =>
            (if Bytes >= 0
            then
              Natural (Bytes) <= R'Length
                and then
              R (R'First .. R'First - 1 + Natural (Bytes))'Valid_Scalars),
            --  The only requirement is that if we want to append more than 1
            --  character, than these characters have to be initialized.
        Contract_Cases =>
          (Bytes <= 0
             or else
           Length (L) = Natural'Last    => To_String (Append'Result)
                                           = To_String (L),
           --  If we want to append 0 characters or less (an error occured),
           --  or if Left has a maximal length, then Left is returned.

           Bytes > 0
             and then
           Natural (Bytes)
           <= Natural'Last - Length (L) => To_String (Append'Result)
                                           = To_String (L)
                                           & To_String
                                               (R (R'First
                                                     ..
                                                   R'First - 1
                                                   + Natural (Bytes))),
           --  Else, if Bytes characters can be appended entirely,
           --  the function returns the Left string appended to the first
           --  Bytes characters of Right.

           others                       => To_String (Append'Result)
                                           = To_String (L)
                                           & To_String
                                               (R (R'First
                                                     ..
                                                   Natural'Last - Length (L)
                                                   - 1 + R'First)))
           --  Else, the Bytes characters cannot be appended entirely, so we
           --  drop the characters that would make the length of the resulting
           --  unbounded string overflow.
           ;

   private

      pragma SPARK_Mode (Off);

      package ASU renames Ada.Strings.Unbounded;

      type Unbounded_String is record
        Str : ASU.Unbounded_String;
      end record;

      function Null_Unbounded_String return Unbounded_String is
        (Unbounded_String'(Str => ASU.Null_Unbounded_String));
      --  Unbounded_String type relies on the Unbounded_String type from
      --  Ada.Strings.Unbounded.

   end Ghost_Package;
end Iostr;
