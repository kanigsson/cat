with Ada.Containers; use Ada.Containers;
with Interfaces.C;   use Interfaces.C;
with Iostr;          use Iostr;
with Ada.Containers.Formal_Hashed_Maps;

--  This package defines the type of variable Contents, that will store
--  the content associated to each file descriptor.

package Contents_Table_Type with
  Ghost,
  SPARK_Mode
is
   use Iostr.Ghost_Package;

   subtype pos_int is int range 0 .. int'Last;
   --  Valid file descriptors

   function Hash_Int (X : pos_int) return Hash_Type is (Hash_Type (X));
   --  Necessary function, here it is identity

   package Formal_Maps is new Ada.Containers.Formal_Hashed_Maps
     (Key_Type        => int,
      Element_Type    => Unbounded_String,
      Hash            => Hash_Int);
   --  Formal_Maps contains the type Map that will be the type of the variable
   --  containing the Contents.
end Contents_Table_Type;
