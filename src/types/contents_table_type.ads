with Ada.Containers; use Ada.Containers;
with Interfaces.C;   use Interfaces.C;
with Iostr;          use Iostr;
with Ada.Containers.Functional_Maps;

package Contents_Table_Type with
  Ghost,
  SPARK_Mode
is

   package Maps is new Ada.Containers.Functional_Maps
     (Key_Type => int,
      Element_Type => One_String,
      Equivalent_Keys => "=",
      "=" => My_Eq,
      Enable_Handling_Of_Equivalence => True);

end Contents_Table_Type;
