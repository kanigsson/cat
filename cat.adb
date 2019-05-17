with Ada.Command_Line;
with Ada.Text_IO;
with Errors;
with Const_H;             use Const_H;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Stdio;               use Stdio;
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
         and then Length (Element (Contents, Input)) = 0,
     Post =>
        M.Same_Keys (Model (Contents'Old), Model (Contents))
          and then
        Element (Contents, Stdout)
      = Element (Contents'Old, Stdout) & Element (Contents, Input)
          and then
        M.Elements_Equal_Except (Model (Contents'Old), Model (Contents), Stdout, Input);

   procedure Copy_To_Stdout (Input : int) is
--        Old_Input, Old_Stdout : Unbounded_String with Ghost;
        Old_Old_Stdout : constant Unbounded_String := Element (Contents, Stdout) with Ghost;
        Contents_Pcd_Entry : constant Map (1023, Default_Modulus (1023)) := Contents with Ghost;
        Contents_Old : Map (1023, Default_Modulus (1023)) := Contents with Ghost;
      -------------------
      --  Append_Equal --
      -------------------

--        procedure Append_Equal with
--          Ghost,
--          Pre =>
--            Old_Stdout = Element (Contents, Stdout)
--              and then
--            Has_Read > 0
--              and then
--            Natural (Has_Read) <= Buf'Length
--              and then
--            Natural (Has_Read) <= NAtural'LAst - Length (Old_Stdout)
--              and then
--            Natural (Has_Read) <= Natural'Last - Length (Element (Contents, Stdout))
--              and then
--            Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars,
--          Post => Append (Old_Stdout, Buf, Has_Read) = Append (Element (Contents, Stdout), BUf, Has_Read);
--
--        procedure Append_Equal is
--
--          Buffer : constant String :=
--             To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read)));
--        begin
--           pragma Assert (To_String (Old_Stdout) = To_String (Element (Contents, Stdout)));
--           String_Append_Equal (To_String (Old_Stdout), To_String (Element (Contents, Stdout)), Buffer, Buffer);
--           pragma Assert (To_String (Old_Stdout) & Buffer
--                          = To_String (Element (Contents, Stdout)) & Buffer);
--           Append_Def (Old_Stdout, Buf, Has_Read);
--           pragma Assert (To_String (Append (Old_Stdout, Buf, Has_Read))
--                          = To_String (Old_Stdout) & To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))));
--           Append_Def (Element (Contents, Stdout), Buf, Has_Read);
--           pragma Assert (To_String (Append (Element (Contents, Stdout), Buf, Has_Read))
--                          = To_String (Element (Contents, Stdout)) & To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))));
--        end Append_Equal;

      --------------------
      -- Prove_Equality --
      --------------------

--        procedure Prove_Equality with
--          Ghost,
--          Pre  =>
--            Input in 0 .. 1023
--              and then Has_Read > 0
--              and then Has_Read = Has_Written
--              and then Has_Read <= Buf'Length
--              and then Length (Old_Input) <= Natural'Last - Length (Old_Old_Stdout)
--              and then Natural (Has_Written) <= Natural'Last - Length (Old_Stdout)
--              and then Natural (Has_Read) <= Natural'Last - Length (Old_Input)
--              and then Length (Element (Contents, Input)) <= Natural'Last - Length (Old_Old_Stdout)
--              and then Has_Read = Has_Written
--              and then Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
--              and then Buf'First = 1
--              and then Old_Stdout = Old_Old_Stdout & Old_Input
--              and then Element (Contents, Stdout) = Append (Old_Stdout, Buf, Has_Written)
--              and then Element (Contents, Input) = Append (Old_Input, Buf, Has_Read),
--          Post => Element (Contents, Stdout) = Old_Old_Stdout & Element (Contents, Input);
--        procedure Prove_Equality is
--        Buffer : constant String :=
--             To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read)));
--        begin
--           Append_Def (Old_Stdout, Buf, Has_Written);
--           Append_Def (Old_Input, Buf, Has_Read);
--           pragma Assert (To_String (Element (Contents, Stdout)) = To_String (Old_Stdout) & Buffer);
--           pragma Assert (To_String (Old_Old_Stdout & Old_Input) = To_String (Old_Old_Stdout) & To_String (Old_Input));
--           pragma Assert (To_String (Element (Contents, Stdout)) = To_String (Old_Old_Stdout) & To_String (Old_Input) & Buffer);
--           pragma Assert (To_String (Element (Contents, Stdout)) = To_String (Old_Old_Stdout) & To_String (Element (Contents, Input)));
--           pragma Assert (Element (Contents, Stdout) = Old_Old_Stdout & Element (Contents, Input));
--        end Prove_Equality;

   begin
--        pragma Assert (Old_Old_Stdout = Element (Contents, Stdout));
      loop
--           Old_Input := Element (Contents, Input);
--           Old_Stdout := Element (Contents, Stdout);

--           pragma Assert (Length (Old_Old_Stdout) <= Natural'Last - Length (Old_Input));
--           U_Append_Def (Old_Old_Stdout, Old_INput);
--           pragma Assert
--             (if Length (Element (Contents, Input)) = 0
--              then Old_Stdout = Old_Old_Stdout
--              else (Old_Stdout = Old_Old_Stdout & Old_Input
--                      and then Old_Stdout = Old_Old_Stdout & Element (Contents, Input)));
         Contents_Old := Contents;
         Read (Input, Buf, Has_Read);
         if Has_Read <= 0 then
            pragma Assert (M.Elements_Equal_Except
                            (Model (Contents),
                             Model (Contents_Pcd_Entry),
                             Input,
                             Stdout));
--              pragma Assert (Old_Input = Element (Contents, Input));
--              String_Append_Equal
--                (To_String (Old_Old_Stdout),
--                 To_String (Old_Old_Stdout),
--                 To_String (Element (Contents, INput)),
--                 To_String (Old_Input));
--              pragma Assert (Old_Stdout = Old_Old_Stdout & Old_INput);
--              pragma Assert (Old_Stdout = Old_Old_Stdout & Element (Contents, Input));
            Has_Written := 0;
--              pragma Assert (Contains (Contents, Stdout));
            exit;
         end if;
         pragma Assert (M.Elements_Equal_Except
                         (Model (Contents),
                          Model (Contents_Old),
                          Input));
--           pragma Assert
--             (Natural (Has_Read) <= NAtural'LAst - Length (Old_Stdout)
--                and then Natural (Has_Read) <= Natural'Last - Length (Element (Contents, Stdout)));
--
--           Append_Equal;
--           pragma Assert (Size_T (Has_Read) <= Buf'Length);
         Contents_Old := Contents;
         Write (Stdout, Buf, Size_T (Has_Read), Has_Written);
         pragma Assert (M.Elements_Equal_Except
                          (Model (Contents),
                           Model (Contents_Old),
                           Stdout));
--           pragma Assert (Natural (Size_T ((Has_Read))) = Natural (Size_T ((Has_Written))));
--           pragma Assert (Natural (Has_Read) = Natural (Has_Written));
--           pragma Assert (Has_Written > 0);
--           pragma Assert (Natural (Has_Written) <= Natural'Last - Length (Old_Stdout));
--           pragma Assert (Element (Contents, Stdout) = Append (Old_Stdout, Buf, Has_Written));

--           pragma Assert
--             (Length (Old_Input) <= Natural'Last - Length (Old_Old_Stdout)
--                and then Natural (Has_Written) <= Natural'Last - Length (Old_Stdout)
--                and then Natural (Has_Read) <= Natural'Last - Length (Old_Input)
--                and then Length (Element (Contents, Input)) <= Natural'Last - Length (Old_Old_Stdout));
--
--           Prove_Equality;
         pragma Loop_Invariant (M.Same_Keys
                                  (Model (Contents'Loop_Entry),
                                   Model (Contents)));
         pragma Loop_Invariant (M.Elements_Equal_Except
                                  (Model (Contents),
                                   Model (Contents'Loop_Entry),
                                   Stdout,
                                   Input));
     end loop;

--        pragma Assert (Length (Old_Old_Stdout) <= Natural'Last - Length (Element (Contents, Input)));
--
        pragma Assert (Element (Contents, Stdout) = Old_Old_Stdout & Element (Contents, Input));
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
            pragma Assert (Length (Element (Contents, Stdin)) = 0);
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
         pragma Loop_Invariant (Length (Element (Contents, Stdin)) = 0);
      end loop;
   end if;
end Cat;
