with Ada.Containers;      use Ada.Containers;
with Const_H;             use Const_H;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
with Ada.Command_Line;
with Errors;
with Full_Write;
with Perror;
with Safe_Read;
with Lemmas;

use Contents_Table_Type.Maps;

procedure Cat with
  SPARK_Mode => On,
  Pre        =>
    (Has_Key (Contents, Stdin)
       and then Has_Key (Contents, Stdout)
       and then Has_Key (Contents, Stderr)
       and then One_String'(Get (Contents, Stdin))'Length = 0
       and then One_String'(Get (Contents, Stdout))'Length = 0
       and then One_String'(Get (Contents, Stderr))'Length = 0)
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
       Has_Key (Contents, Stdout)
         and then Has_Key (Contents, Input)
         and then Input >= 0
         and then Input /= Stdout
         and then One_String'(Get (Contents, Input))'Length = 0,
     Post =>
       Same_Keys (Contents'Old, Contents)
         and then
       Elements_Equal_Except (Contents'Old,
                              Contents,
                              Input,
                              Stdout)
       --  Contents is the same as before except for the content in
       --  key Input and Stdout.
         and then
         --  If no error occured, then what we read from Input has been
         --  copied to Stdout.
      (if Err = 0
       then
           Is_Append (Get (Contents'Old, Stdout), Get (Contents, Input),
                      Get (Contents, Stdout)));

   ----------------------
   --  Copy_To_Stdout  --
   ----------------------

   procedure Copy_To_Stdout (Input : int; Err : out int) is
      Contents_Pcd_Entry : constant Map :=
        Contents with Ghost;
      --  Stores the value of Contents at the beginning of the procedure
      Contents_Old : Map with Ghost;
      --  Updated at the beginning of each iteration of the loop to store the
      --  value of Contents at the end of last iteration.
      Contents_Tmp : Map with Ghost;
      --  Represents the intermediate map after read, but before write.
      Buf : Init_String (1 .. 1024);
      --  Buffer

      Has_Read : ssize_t;
      --  Parameter of Safe_Read
   begin
      loop
         Contents_Old := Contents;
         pragma Assert (Elements_Equal_Except (Contents_Pcd_Entry, Contents,
                        Input, Stdout));
         --  Value of Contents at the beginning of the loop is stored in
         --  Contents_Old.

         Safe_Read (Input, Buf, Has_Read);
         --  Data is read from Input

         if Has_Read = 0 then
            Lemmas.Lemma_Equal_Except2_Id
              (Contents_Pcd_Entry, Contents_Old, Contents, Input, Stdout);
            exit;

         elsif Has_Read = -1 then
            Err := -1;
            pragma Assert (Contents_Old = Contents);
            Lemmas.Lemma_Equal_Except2_Id
              (Contents_Pcd_Entry, Contents_Old, Contents, Input, Stdout);
            pragma Assert (Elements_Equal_Except (Contents_Pcd_Entry, Contents,
                           Input, Stdout));
            return;
            --  If Has_Read = 1, then an error occured

         end if;

         Contents_Tmp := Contents;
         Full_Write
           (Stdout,
            Buf,
            size_t (Has_Read),
            Err);
         if Err = -1 then
            Lemmas.Lemma_Equal_Except21_Trans
              (Contents_Pcd_Entry, Contents_Tmp, Contents, Input, Stdout);
            pragma Assert (Elements_Equal_Except (Contents_Pcd_Entry, Contents,
                           Input, Stdout));
            return;
            --  If Err = -1, then an error occured.

         end if;

         pragma Assert
           (Elements_Equal_Except (Contents_Old, Contents_Tmp, Input));
         pragma Assert
           (Elements_Equal_Except (Contents_Tmp, Contents, Stdout));
         Lemmas.Lemma_Equal_Except_Trans
           (Contents_Old, Contents_Tmp, Contents, Input, Stdout);
         pragma Assert
           (Elements_Equal_Except (Contents_Old, Contents, Input, Stdout));
         Lemmas.Lemma_Equal_Except2_Trans
           (Contents_Pcd_Entry, Contents_Old, Contents, Input, Stdout);
         Lemmas.Lemma_Is_Append_Equal_Except_Inv
           (Contents_Old, Contents_Tmp, Contents,
            Input, Stdout, Buf, Has_Read);
         Lemmas.Lemma_Is_Append_Equal_Except_Inv2
           (Contents_Old, Contents_Tmp, Contents,
            Input, Stdout, Buf, Has_Read);
         Lemmas.Lemma_Is_Append_Trans (Get (Contents_Pcd_Entry, Stdout),
                                       Get (Contents_Old, Input),
                                       Get (Contents, Input),
                                       Get (Contents_Old, Stdout),
                                       Get (Contents, Stdout),
                                       Buf, Has_Read);


         pragma Loop_Invariant (Same_Keys
                                (Contents_Pcd_Entry,
                                   Contents));
         pragma Loop_Invariant (Elements_Equal_Except
                                (Contents_Pcd_Entry,
                                 Contents,
                                 Input,
                                 Stdout));
         pragma Loop_Invariant (Is_Append (Get (Contents_Pcd_Entry, Stdout),
                                           Get (Contents, Input),
                                           Get (Contents, Stdout)));
      end loop;
      pragma Assert (Elements_Equal_Except (Contents_Pcd_Entry, Contents,
                                            Input, Stdout));
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
               pragma Assert (One_String'(Get (Contents, Stdin))'Length = 0);
               if Err = -1 then
                  Perror (Err_Message (Ada.Command_Line.Argument (I)));
               end if;
            else
               Reset (Stdin);
               pragma Assert (One_String'(Get (Contents, Stdin))'Length = 0);
            end if;
            --  This block closes the file or resets the content of Stdin.
         end if;

         pragma Assert (X /= Stdout);
         pragma Assert (Has_Key (Contents, Stdout));
         pragma Loop_Invariant (Has_Key (Contents, Stdout));
         pragma Loop_Invariant (Has_Key (Contents, Stdin));
         pragma Loop_Invariant (One_String'(Get (Contents, Stdin))'Length = 0);
         --  These loop invariants allow to prove Copy_To_Stdout preconditions.
      end loop;
   end if;
end Cat;
