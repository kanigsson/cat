with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Interfaces.C;   use Interfaces.C;
with Iostr;          use Iostr;

package Contents_Table_Type with
  SPARK_Mode
is
   use Iostr.Ghost_Package;

   subtype pos_int is Int range 0 .. Int'Last;

   function Hash_Int (X : pos_int) return Hash_Type is (Hash_Type (X));
   package Formal_Maps is new Ada.Containers.Formal_Hashed_Maps
     (Key_Type        => int,
      Element_Type    => Unbounded_String_Holder,
      Hash            => Hash_Int,
      Equivalent_Keys => "=");
end Contents_Table_Type;
