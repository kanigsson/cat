with Interfaces.C; use Interfaces.C;

package Init_Strings with
  SPARK_Mode
is
   subtype ssize_t is int;

   subtype Init_Char is Character;
   pragma Annotate(GNATprove, Init_By_Proof, Init_Char);

   type Init_String is array (Positive range <>) of Init_Char;
end Init_Strings;
