with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Stdio;        use Stdio;
with Iostr;        use Iostr;
with Errors;
with Const_H;      use Const_H;
with Init_Strings; use Init_Strings;

procedure Cat with
  SPARK_Mode
is

   X : int;
   Buf : Init_String (1 .. 1024);
   Has_Read, Has_Written : ssize_t;

   procedure Copy_To_Stdout (Input : int) with
     Global => (Output => (Has_Read, Has_Written, Buf),
                In_Out => (FD_Table, Contents, Errors.Error_State)),
     Pre  => Input in 0 .. 1023 and then Input /= Stdout,
     Post => Contents (Stdout) = Contents'Old (Stdout) & Contents (Input);

   procedure Copy_To_Stdout (Input : int) is
      Err : int;
      Old_Input, Old_Stdout : Unbounded_String with Ghost;
      Old_Old_Stdout : constant Unbounded_String := Contents (Stdout) with Ghost;

      -----------------
      --  Append_Def --
      -----------------

      procedure Append_Def
        (Left  : Unbounded_String;
         Right : Init_String;
         Bytes : Int)
      with
        Ghost,
        Pre =>
          Bytes > 0
            and then
          Natural (Bytes) <= Right'Length
            and then
          Natural (Bytes) <= NAtural'LAst - Length (Left)
            and then
          Right (Right'First.. Right'First - 1 + Natural (Bytes))'Valid_Scalars,
        Post =>
          To_String (Append (Left, Right, Bytes))
        = To_String (Left)
          & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes)));

      procedure Append_Def
        (Left  : Unbounded_String;
         Right : Init_String;
         Bytes : Int)
      is
      begin
         pragma Assert (Bytes > 0);
         pragma Assert (To_String (Append (Left, Right, Bytes))
                        = To_String (Left)
                        & To_String (Right (Right'First .. Right'First - 1 + Natural (Bytes))));
      end Append_Def;

      -------------------
      --  Append_Equal --
      -------------------
-- Append_Equal (Old_Stdout, Contents (Stdout), Buf, Has_Read);
      procedure Append_Equal with
        Ghost,
        Pre =>
          Old_Stdout = Contents (Stdout)
            and then
          Has_Read > 0
            and then
          Natural (Has_Read) <= Buf'Length
            and then
          Natural (Has_Read) <= NAtural'LAst - Length (Old_Stdout)
            and then
          Natural (Has_Read) <= Natural'Last - Length (Contents (Stdout))
            and then
          Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars,
        Post => Append (Old_Stdout, Buf, Has_Read) = Append (Contents (Stdout), BUf, Has_Read);

      procedure Append_Equal is

         procedure String_Append_Equal (A, B, C : String) with
           Pre  =>
             A = B
             and then A'First = 1
             and then B'First = 1
             and then C'Length <= Natural'Last - A'Length,
           Post => A & C = B & C;
         procedure String_Append_Equal (A, B, C : String) is null;

        Buffer : constant String :=
           To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read)));
      begin
         pragma Assert (To_String (Old_Stdout) = To_String (Contents (Stdout)));
         String_Append_Equal (To_String (Old_Stdout), To_String (Contents (Stdout)), Buffer);
         pragma Assert (To_String (Old_Stdout) & Buffer
                        = To_String (Contents (Stdout)) & Buffer);
         Append_Def (Old_Stdout, Buf, Has_Read);
         pragma Assert (To_String (Append (Old_Stdout, Buf, Has_Read))
                        = To_String (Old_Stdout) & To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))));
         Append_Def (Contents (Stdout), Buf, Has_Read);
         pragma Assert (To_String (Append (Contents (Stdout), Buf, Has_Read))
                        = To_String (Contents (Stdout)) & To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))));
      end Append_Equal;

      --------------------
      -- Prove_Equality --
      --------------------

      procedure Prove_Equality with
        Ghost,
        Pre  =>
          Input in 0 .. 1023
            and then
          Has_Read > 0
            and then
          Has_Read = Has_Written
            and then
          Has_Read <= Buf'Length
            and then
          Length (Old_Input) <= Natural'Last - Length (Old_Old_Stdout)
            and then
          Natural (Has_Written) <= Natural'Last - Length (Old_Stdout)
            and then
          Natural (Has_Read) <= Natural'Last - Length (Old_Input)
            and then
          Length (Contents (Input)) <= Natural'Last - Length (Old_Old_Stdout)
            and then
          Has_Read = Has_Written
            and then
          Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
            and then
          Buf'First = 1
            and then
          Old_Stdout = Old_Old_Stdout & Old_Input
            and then
          Contents (Stdout) = Append (Old_Stdout, Buf, Has_Written)
            and then
          Contents (Input) = Append (Old_Input, Buf, Has_Read),
        Post =>
          Contents (Stdout) = Old_Old_Stdout & Contents (Input);
      procedure Prove_Equality is
      Buffer : constant String :=
           To_String (Buf (Buf'First.. Buf'First - 1 + Natural (Has_Read)));
      begin
         Append_Def (Old_Stdout, Buf, Has_Written);
         Append_Def (Old_Input, Buf, Has_Read);
         pragma Assert (To_String (Contents (Stdout)) = To_String (Old_Stdout) & Buffer);
         pragma Assert (To_String (Old_Stdout) = To_String (Old_Old_Stdout) & To_String (Old_Input));
         pragma Assert (To_String (Contents (Stdout)) = To_String (Old_Old_Stdout) & To_String (Old_Input) & Buffer);
         pragma Assert (To_String (Contents (Stdout)) = To_String (Old_Old_Stdout) & To_String (Contents (Input)));
         pragma Assert (Contents (Stdout) = Old_Old_Stdout & Contents (Input));
      end Prove_Equality;

   begin
      loop
         Old_Input := Contents (Input);
         Old_Stdout := Contents (Stdout);
         pragma Assert (Old_Stdout = Old_Old_Stdout & Old_Input);
         Read (Input, Buf, Has_Read);

         if Has_Read <= 0 then
            pragma Assert (Old_Input = Contents (Input));
            pragma Assert (Contents (Stdout) = Old_Old_Stdout & Contents (Input));
            Has_Written := 0;
            exit;
         end if;

         pragma Assert
           (Natural (Has_Read) <= NAtural'LAst - Length (Old_Stdout)
              and then Natural (Has_Read) <= Natural'Last - Length (Contents (Stdout)));

         Append_Equal;
         pragma Assert (Size_T (Has_Read) <= Buf'Length);
         Write (Stdout, Buf, Size_T (Has_Read), Has_Written);
         pragma Assert (Natural (Size_T ((Has_Read))) = Natural (Size_T ((Has_Written))));
         pragma Assert (Natural (Has_Read) = Natural (Has_Written));
         pragma Assert (Has_Written > 0);
         pragma Assert (Natural (Has_Written) <= Natural'Last - Length (Old_Stdout));
         pragma Assert (Contents (Stdout) = Append (Old_Stdout, Buf, Has_Written));

         pragma Assert
           (Length (Old_Input) <= Natural'Last - Length (Old_Old_Stdout)
              and then Natural (Has_Written) <= Natural'Last - Length (Old_Stdout)
              and then Natural (Has_Read) <= Natural'Last - Length (Old_Input)
              and then Length (Contents (Input)) <= Natural'Last - Length (Old_Old_Stdout));

         Prove_Equality;
         pragma Loop_Invariant (Contents (Stdout) = Old_Old_Stdout & Contents (Input));
      end loop;

      pragma Assert (Length (Old_Old_Stdout) <= Natural'Last - Length (Contents (Input)));

      pragma Assert (Contents (Stdout) = Old_Old_Stdout & Contents (Input));
      Close (Input, Err);
   end Copy_To_Stdout;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Copy_To_Stdout (Stdin);
   else
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "-" then
            X := Stdin;
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
         end if;
         Copy_To_Stdout (X);
      end loop;
   end if;
end Cat;
