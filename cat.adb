with Ada.Command_Line;
with Ada.Text_IO;
with Errors;
with Const_H;             use Const_H;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Lemmas; use LEmmas;
use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type. Formal_Maps.Formal_Model;
use Iostr.Ghost_Package;

procedure Cat with
  SPARK_Mode => On,
  Pre        =>
    (Contains (Contents, Stdin)
       and then Contains (Contents, Stdout)
       and then Contains (Contents, Stderr)
       and then Length (Element (Contents, Stdin).String) = 0
       and then Length (Element (Contents, Stdout).String) = 0
       and then Length (Element (Contents, Stderr).String) = 0)
is
   X : int;
   Err : int;
   Buf : Init_String (1 .. 1024);
   Has_Read, Has_Written : ssize_t;

   procedure Copy_To_Stdout (Input : int) with
     Global => (Proof_In => (FD_Table),
                Output   => (Has_Read, Has_Written, Buf),
                In_Out   => (Contents, Errors.Error_State)),
     Pre    =>
       Contains (Contents, Stdout)
         and then Input >= 0
         and then Contains (Contents, Input)
         and then Input /= Stdout
         and then Length (Element (Contents, Input).String) = 0,
     Post =>
        M.Same_Keys (Model (Contents'Old), Model (Contents))
          and then
        Element (Contents, Stdout).String
      = Element (Contents'Old, Stdout).String & Element (Contents, Input).String
          and then
        M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Stdout, Input);

   procedure Copy_To_Stdout (Input : int) is
      Contents_Pcd_Entry : constant Map (1023, Default_Modulus (1023)) := Contents with Ghost;
      Contents_Old : Map (1023, Default_Modulus (1023)) := Contents with Ghost;
      Old_Stdout, Old_Input : Unbounded_String with Ghost;
   begin

      pragma Assert (M.Elements_Equal_Except
                      (Model (Contents),
                       MOdel (Contents_Pcd_Entry),
                       Input,
                       Stdout));
      pragma Assert (Element (Contents_Old, Stdout).String
                     = Element (Contents_Pcd_Entry, Stdout).String
                     & Element (Contents_Old, Input).String);
      loop
         Contents_Old := Contents;
            pragma Assert (Element (Contents_Old, Stdout).String
                           = Element (Contents_Pcd_Entry, Stdout).String
                           & Element (Contents_Old, Input).String);
         Read (Input, Buf, Has_Read);
         pragma Assert (Element (Contents, Stdout).String
                        = Element (Contents_Old, Stdout).String);
         pragma Assert (M.Elements_Equal_Except
                         (Model (Contents),
                          Model (Contents_Pcd_Entry),
                          Input,
                          Stdout));

         pragma Assert (Element (Contents, Input).String
                        = Append (Element (Contents_Old, INput).String,
                                  Buf,
                                  Has_Read));

         if Has_Read <= 0 then
            Equal_String (Element (Contents, Stdout).String,
                          Element (Contents_Old, Stdout).String,
                          Element (Contents_Pcd_Entry, Stdout).String,
                          Element (Contents_Old, Input).String);
            Equal_And_Append (Element (Contents, Stdout).String,
                              Element (Contents_Pcd_Entry, Stdout).String,
                              Element (Contents_Old, Input).String,
                              Element (Contents, Input).String);
           Has_Written := 0;
            exit;
         end if;

         Old_Stdout := Element (Contents, Stdout).String;
         pragma Assert (Size_T (Has_Read) <= Buf'Length);
         Write (Stdout, Buf, Size_T (Has_Read), Has_Written);

         pragma Assert (Natural (Size_T (Has_Read)) = Natural (Size_T (Has_Written)));
         pragma Assert (Natural (Has_Read) = NAtural (Has_Written));
         Equal_And_Append (Element (Contents, Stdout).String,
                           Old_Stdout,
                           ELement (Contents_Old, Stdout).String,
                           Buf,
                           Has_Written);
         Prove_Equality (Contents, Contents_Old, Contents_Pcd_Entry, Buf, Has_Read, Has_Written, Input, Stdout);

         pragma Loop_Invariant (M.Same_Keys
                                  (Model (Contents_Pcd_Entry),
                                   Model (Contents)));
         pragma Loop_Invariant (M.Elements_Equal_Except
                                  (Model (Contents),
                                   Model (Contents_Pcd_Entry),
                                   Stdout,
                                   Input));
         pragma Loop_Invariant (Element (Contents, Stdout).String
                                = Element (Contents_Pcd_Entry, Stdout).String
                                & Element (Contents, Input).String);
     end loop;

        pragma Assert (Element (Contents, Stdout).String
                       = Element (Contents_Pcd_Entry, Stdout).String
                       & Element (Contents, Input).String);
   end Copy_To_Stdout;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Copy_To_Stdout (Stdin);
   else
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "-" then
            X := Stdin;
            Copy_To_Stdout (X);
            Reset (Stdin);
            pragma Assert (X /= Stdout);
            pragma Assert (Contains (Contents, Stdout));
            pragma Assert (Length (Element (Contents, Stdin).String) = 0);
         else
            Open (To_C (Ada.Command_Line.Argument (I)), ADA_O_RDONLY, X);
            if X = -1 then
               case Errors.Get_Errno is
               when Errors.ADA_ENOENT =>
                  Ada.Text_IO.Put_Line ("file does not exist");
               when others =>
                  Ada.Text_IO.Put_Line ("unknown errors");
               end case;
               return;
            end if;
            Copy_To_Stdout (X);
            Close (X, Err);
            Reset (Stdin);     --  find a solution to not do this: useless
                               --  in this case except for proof.
              pragma Assert (X /= Stdout);
              pragma Assert (Contains (Contents, Stdout));
            if Err = -1 then
               case Errors.Get_Errno is
               when others =>
                  Ada.Text_IO.Put_Line ("error when closing file");
               end case;
            end if;
         end if;
         pragma Loop_Invariant (Contains (Contents, Stdout));
         pragma Loop_Invariant (Contains (Contents, Stdin));
         pragma Loop_Invariant (Length (Element (Contents, Stdin).String) = 0);
      end loop;
   end if;
end Cat;
