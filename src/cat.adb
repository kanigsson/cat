with Ada.Containers;      use Ada.Containers;
with Const_H;             use Const_H;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Lemmas;              use Lemmas;
with Stdio;               use Stdio;
with Ada.Command_Line;
with Errors;
with Full_Write;
with Perror;
with Safe_Read;

use Contents_Table_Type.Formal_Maps;
use Contents_Table_Type. Formal_Maps.Formal_Model;
use Iostr.Ghost_Package;

procedure Cat with
  SPARK_Mode => On,
  Pre        =>
    (Contains (Contents, Stdin)
       and then Contains (Contents, Stdout)
       and then Contains (Contents, Stderr)
       and then Length (Element (Contents, Stdin)) = 0
       and then Length (Element (Contents, Stdout)) = 0
       and then Length (Element (Contents, Stderr)) = 0)
is
   X : int;
   Err : int;

   --  Err_Message generates the argument of Perror when it is called.

   function Err_Message (Str : String) return String is
     (case Str'Length is
      when 0 .. Natural'Last - 5 => "cat: " & Str,
      when others                =>
        "cat: " & Str (Str'First .. Natural'Last - 6 + Str'First));

   --  Copy_To_Stdout writes to Stdout what it reads in Input until
   --  it reaches the end of file.

   procedure Copy_To_Stdout (Input : int; Err : out int) with
     Global => (Proof_In => (FD_Table),
                In_Out   => (Contents, Errors.Error_State)),
     Pre    =>
       Contains (Contents, Stdout)
         and then Contains (Contents, Input)
         and then Input >= 0
         and then Input /= Stdout
         and then Length (Element (Contents, Input)) = 0,
     Post =>
       M.Same_Keys (Model (Contents'Old), Model (Contents))
         and then
       M.Elements_Equal_Except (Model (Contents),
                                Model (Contents'Old),
                                Input,
                                Stdout)
       --  Contents is the same as before except for the content in
       --  key Input and Stdout.

         and then
      (if Err = 0
       then
         --  If no error occured, then what we read from Input has been
         --  copied to Stdout.
         Element (Contents, Stdout)
         = Element (Contents'Old, Stdout)
         & Element (Contents, Input));

   ----------------------
   --  Copy_To_Stdout  --
   ----------------------

   procedure Copy_To_Stdout (Input : int; Err : out int) is
      Contents_Pcd_Entry : constant Map (OPEN_MAX - 1,
                                         Default_Modulus (OPEN_MAX - 1)) :=
        Contents with Ghost;
      --  Stores the value of Contents at the beginning of the procedure

      Contents_Old : Map (OPEN_MAX - 1, Default_Modulus (OPEN_MAX - 1))  :=
        Contents with Ghost;
      --  Updated at the beginning of each iteration of the loop to store the
      --  value of Contents at the end of last iteration.

      Old_Stdout : Unbounded_String with Ghost;
      --  Stores the value of Element (Contents, Stdout) before the call
      --  to Full_Write.

      Buf : Init_String (1 .. 1024);
      --  Buffer

      Has_Read : ssize_t;
      --  Parameter of Safe_Read
   begin
      pragma Assert (M.Elements_Equal_Except
                      (Model (Contents),
                       Model (Contents_Pcd_Entry),
                       Input,
                       Stdout));
      pragma Assert (Element (Contents_Old, Stdout)
                     = Element (Contents_Pcd_Entry, Stdout)
                     & Element (Contents_Old, Input));
      loop
         Contents_Old := Contents;
         pragma Assert (Element (Contents_Old, Stdout)
                        = Element (Contents_Pcd_Entry, Stdout)
                        & Element (Contents_Old, Input));
         --  Value of Contents at the beginning of the loop is stored in
         --  Contents_Old.

         Safe_Read (Input, Buf, Has_Read);
         --  Data is read from Input

         pragma Assert (Element (Contents, Stdout)
                        = Element (Contents_Old, Stdout));
         pragma Assert (M.Elements_Equal_Except
                         (Model (Contents),
                          Model (Contents_Pcd_Entry),
                          Input,
                          Stdout));
         pragma Assert (M.Same_Keys (Model (Contents_Pcd_Entry),
                                     Model (Contents)));
         pragma Assert (Element (Contents, Input)
                        = Append (Element (Contents_Old, Input),
                                  Buf,
                                  Has_Read));
         --  The assertions above are postconditions of Safe_Read

         if Has_Read = 0 then
            Equality_Transitivity (Element (Contents, Stdout),
                          Element (Contents_Old, Stdout),
                          Element (Contents_Pcd_Entry, Stdout),
                          Element (Contents_Old, Input));
            Right_Substitution_In_Concat (Element (Contents, Stdout),
                              Element (Contents_Pcd_Entry, Stdout),
                              Element (Contents_Old, Input),
                              Element (Contents, Input));
            exit;
            --  If Has_Read = 0, then End_Of_File is reached. The two Ghost
            --  procedures are called to prove postcondition of Copy_To_Stdout.

         elsif Has_Read = -1 then
            Err := -1;
            return;
            --  If Has_Read = 1, then an error occured

         end if;

         Old_Stdout := Element (Contents, Stdout);
         --  Old_Stdout is initialized to keep track of the value of
         --  Element (Contents, Stdout) before the call to Full_Write.

         pragma Assert (size_t (Has_Read) <= Buf'Length);
         --  Helps to prove precondition of Full_Write

         Full_Write
           (Stdout,
            Buf,
            size_t (Has_Read),
            Err);

         pragma Assert (M.Elements_Equal_Except
                        (Model (Contents),
                           Model (Contents_Pcd_Entry),
                           Input,
                           Stdout));
         --  The postconditions of Full_Write and Safe_Read combined produce
         --  this property.

         if Err = -1 then
            return;
            --  If Err = -1, then an error occured.

         end if;

         Left_Substitution_In_Concat (Element (Contents, Stdout),
                           Old_Stdout,
                           Element (Contents_Old, Stdout),
                           Buf,
                           Has_Read);
         --  This helps to prove the precondition of Prove_Copy_To_Stdout_LI

         Prove_Copy_To_Stdout_LI (Contents,
                         Contents_Old,
                         Contents_Pcd_Entry,
                         Buf,
                         Has_Read,
                         Input,
                         Stdout);
         --  This procedure helps to prove the third loop invariant

         pragma Loop_Invariant (M.Same_Keys
                                  (Model (Contents_Pcd_Entry),
                                   Model (Contents)));
         --  Postcondition
         pragma Loop_Invariant (M.Elements_Equal_Except
                                  (Model (Contents),
                                   Model (Contents_Pcd_Entry),
                                   Stdout,
                                   Input));
         --  Postcondition
         pragma Loop_Invariant (Element (Contents, Stdout)
                                = Element (Contents_Pcd_Entry, Stdout)
                                & Element (Contents, Input));
         --  Postcondition when Err = 0
      end loop;

      Err := 0;
      --  If we exit the loop, everything went fine

   end Copy_To_Stdout;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Copy_To_Stdout (Stdin, Err);
      --  If no file is specified, we copy the content from Stdin

      if Err = -1 then
         Perror ("cat: ");
      end if;
   else

      for I in 1 .. Ada.Command_Line.Argument_Count loop
      --  Else, we loop on the files called in argument

         if Ada.Command_Line.Argument (I) = "-" then
            X := Stdin;
         else
            Open (To_C (Ada.Command_Line.Argument (I)), ADA_O_RDONLY, X);
            if X = -1 then
               Perror (Err_Message (Ada.Command_Line.Argument (I)));
            end if;
         end if;
         --  This block opens the file if necessary, and assigns X to the FD

         if X /= -1 then
            Copy_To_Stdout (X, Err);
            --  Copying the content to Stdout

            if Err = -1 then
               Perror (Err_Message (Ada.Command_Line.Argument (I)));
            end if;

            if X /= Stdin then
               Close (X, Err);

               if Err = -1 then
                  Perror (Err_Message (Ada.Command_Line.Argument (I)));
               end if;
            else
               Reset (Stdin);
            end if;
            --  This block closes the file or resets the content of Stdin.
         end if;

         pragma Assert (X /= Stdout);
         pragma Assert (Contains (Contents, Stdout));
         pragma Loop_Invariant (Contains (Contents, Stdout));
         pragma Loop_Invariant (Contains (Contents, Stdin));
         pragma Loop_Invariant (Length (Element (Contents, Stdin)) = 0);
         --  These loop invariants allow to prove Copy_To_Stdout preconditions.

      end loop;
   end if;
end Cat;
