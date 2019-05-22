with Errors;
with Ada.Containers;      use Ada.Containers;
with Const_H;             use Const_H;
with Interfaces.C;        use Interfaces.C;
with Contents_Table_Type; use Contents_Table_Type;
with Iostr;               use Iostr;


package Stdio with
  SPARK_Mode        => On,
  Abstract_State    => (FD_Table),
  Initializes       => (FD_Table,
                        Contents),
  Initial_Condition =>
    (Contains (Contents, Stdin)
       and then Contains (Contents, Stdout)
       and then Contains (Contents, Stderr)
       and then Length (Element (Contents, Stdin).String) = 0
       and then Length (Element (Contents, Stdout).String) = 0
       and then Length (Element (Contents, Stderr).String) = 0)
is

   pragma Elaborate_Body;
   
   use Iostr.Ghost_Package;
   use Formal_Maps;
   use Formal_Maps.Formal_Model;
   
   Contents : Map (1023, Default_Modulus (1023)) with Ghost;
   
   subtype off_t is int;

   procedure Open (File : char_array; Flags : int; Fd : out Int) with
     Global => (In_Out => (FD_Table,Errors.Error_State, Contents)),
     Post   =>
       (Fd = -1 and then Contents'Old = Contents)
          or else
       (Fd >= 0
          and then
        Length (Contents'Old) + 1 = Length (Contents)
          and then
        Contains (Contents, Fd)
          and then
        Length (Element (Contents, Fd).String) = 0
          and then
        not Contains (Contents'Old, Fd)
          and then
        Model (Contents'Old) <= Model (Contents)
          and then
        M.Keys_Included_Except (Model (Contents), Model (Contents'Old), Fd));

   procedure Close (Fd : int; Result : out Int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
       (Result = -1 and then Contents = Contents'Old)
          or else
       (Fd = 0
          and then
        Length (Contents) = Length (Contents'Old) - 1
          and then
        not Contains (Contents, Fd)
          and then
        Model (Contents) <= Model (Contents'Old)
          and then
        M.Keys_Included_Except (Model (Contents'Old), Model (Contents), Fd));

   function Has_Reading (Flags : int) return Boolean is
     (Flags mod 4 in Const_H.ADA_O_RDWR | Const_H.ADA_O_RDONLY);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => (FD_Table, Const_H.ADA_O_RDONLY, Const_H.ADa_O_RDWR)),
     Post =>
       Has_Read <= Buf'Length
          and then
       (if Has_Read <= 0
          then Contents = Contents'Old
        elsif Has_Read > 0
          then
            (Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Valid_Scalars
               and then
             Contains (Contents, Fd)
               and then
             M.Same_Keys (Model (Contents), Model (Contents'Old))
               and then
             Element (Contents, Fd).String = Append (Element (Contents'Old, Fd).String, Buf, Has_Read)
               and then
             M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd)));

   procedure Write
     (Fd          : int;
      Buf         : in Init_String;
      Num_Bytes   : Size_T;
      Has_Written : out ssize_t)
   with
     Global => (In_Out => (Errors.Error_State, Contents)),
     Pre    =>
       (Num_Bytes <= Buf'Length
         and then Num_Bytes > 0
         and then Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_scalars),
     Post   =>
       Num_Bytes = Size_T (Has_Written)
         and then
       Natural (Has_Written) <= Buf'Length
         and then 
       Contains (Contents, Fd)
         and then
       M.Same_Keys (Model (Contents), Model (Contents'Old))
         and then
       Element (Contents, Fd).String = Append (Element (Contents'Old, Fd).String, Buf, Has_Written)
         and then
       M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd);
       
   procedure Reset (Fd : int) with
     Ghost,
     Global => (In_Out => Contents),
     Post   =>
       (if not Contains (Contents'Old, Fd)
          then Contents'Old = Contents
        else
          M.Same_Keys (Model (Contents), Model (Contents'Old))
            and then
          M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd)
            and then
          Length (Element (Contents, Fd).String) = 0);


   Stdin  : constant int := 0;
   Stdout : constant int := 1;
   Stderr : constant int := 2;

end Stdio;
