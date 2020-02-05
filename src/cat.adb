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

   function Err_Message (Str : String) return String is
     (case Str'Length is
      when 0 .. Natural'Last - 5 => "cat: " & Str,
      when others                =>
        "cat: " & Str (Str'First .. Natural'Last - 6 + Str'First));

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
       Elements_Equal_Except (Contents,
                                Contents'Old,
                                Input,
                                Stdout)
         and then
      (if Err = 0
       then
         Get (Contents, Stdout)
         = Get (Contents'Old, Stdout)
         & Get (Contents, Input));

   procedure Copy_To_Stdout (Input : int; Err : out int) is
      Contents_Pcd_Entry : constant Map :=
        Contents with Ghost;
      Contents_Old : Map := Contents with Ghost;
      Buf : Init_String (1 .. 1024);
      Has_Read : ssize_t;
   begin
      pragma Assert (Elements_Equal_Except
                      (Contents,
                       Contents_Pcd_Entry,
                       Input,
                       Stdout));
      pragma Assert (Get (Contents_Old, Stdout)
                     = Get (Contents_Pcd_Entry, Stdout)
                     & Get (Contents_Old, Input));
      loop
         Contents_Old := Contents;
            pragma Assert (Get (Contents_Old, Stdout)
                           = Get (Contents_Pcd_Entry, Stdout)
                           & Get (Contents_Old, Input));

         Safe_Read (Input, Buf, Has_Read);
         pragma Assert (Get (Contents, Stdout)
                        = Get (Contents_Old, Stdout));
         pragma Assert (Elements_Equal_Except
                         (Contents,
                          Contents_Pcd_Entry,
                          Input,
                          Stdout));
         pragma Assert (Same_Keys (Contents_Pcd_Entry,
                                     Contents));
         pragma Assert (Is_Append (Get (Contents_Old, Input), String (Buf),
                        Get (Contents, Input), Has_Read));
         if Has_Read = 0 then
            exit;
         elsif Has_Read = -1 then
            Err := -1;
            return;
         end if;

         declare
            Old_Stdout : One_String := Get (Contents, Stdout) with Ghost;
         begin
            pragma Assert (size_t (Has_Read) <= Buf'Length);
            Full_Write
              (Stdout,
               Buf,
               size_t (Has_Read),
               Err);
            pragma Assert (Elements_Equal_Except
                           (Contents,
                              Contents_Pcd_Entry,
                              Input,
                              Stdout));
            if Err = -1 then
               return;
            end if;

            pragma Loop_Invariant (Same_Keys
                                   (Contents_Pcd_Entry,
                                      Contents));
            pragma Loop_Invariant (Elements_Equal_Except
                                   (Contents,
                                      Contents_Pcd_Entry,
                                      Stdout,
                                      Input));
            pragma Loop_Invariant (Get (Contents, Stdout)
                                   = Get (Contents_Pcd_Entry, Stdout)
                                   & Get (Contents, Input));
         end;
      end loop;

      Err := 0;
   end Copy_To_Stdout;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Copy_To_Stdout (Stdin, Err);
      if Err = -1 then
         Perror ("cat: ");
      end if;
   else
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "-" then
            X := Stdin;
         else
            Open (To_C (Ada.Command_Line.Argument (I)), ADA_O_RDONLY, X);
            if X = -1 then
               Perror (Err_Message (Ada.Command_Line.Argument (I)));
            end if;
         end if;

         if X /= -1 then
            Copy_To_Stdout (X, Err);

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
         end if;

         pragma Assert (X /= Stdout);
         pragma Assert (Has_Key (Contents, Stdout));
         pragma Loop_Invariant (Has_Key (Contents, Stdout));
         pragma Loop_Invariant (Has_Key (Contents, Stdin));
         pragma Loop_Invariant (One_String'(Get (Contents, Stdin))'Length = 0);
      end loop;
   end if;
end Cat;
