with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Interfaces.C;   use Interfaces.C;
with Iostr;          use Iostr;

package Contents_Table_Type with
  Ghost,
  SPARK_Mode
is
   use Iostr.Ghost_Package;
   function Hash_Int (X : Int) return Hash_Type is (Hash_Type (X));
   package Formal_Maps is new Ada.Containers.Formal_Hashed_Maps
     (Key_Type        => int,
      Element_Type    => Unbounded_String,
      Hash            => Hash_Int,
      Equivalent_Keys => "=");
end Contents_Table_Type;
