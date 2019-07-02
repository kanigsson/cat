with Ada.Containers;      use Ada.Containers;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Errors;

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

   OPEN_MAX : constant Count_Type := 10298495;

   Contents : Map (OPEN_MAX - 1, Default_Modulus (OPEN_MAX - 1)) with Ghost;

   subtype off_t is int;

   procedure Open (File : char_array; Flags : int; Fd : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
       (case Fd is
          when -1                      => Contents'Old = Contents,
          when 0 .. int (OPEN_MAX - 1) =>
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
            M.Keys_Included_Except (Model (Contents),
                                    Model (Contents'Old),
                                    Fd),
          when others                  => False);

   procedure Close (Fd : int; Result : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
     (case Result is
       when -1     => Contents = Contents'Old,
       when 0      =>
          Length (Contents) = Length (Contents'Old) - 1
            and then
          not Contains (Contents, Fd)
            and then
          Model (Contents) <= Model (Contents'Old)
            and then
          M.Keys_Included_Except (Model (Contents'Old), Model (Contents), Fd),
       when others => False);

   procedure Read (Fd : int; Buf : out Init_String; Has_Read : out ssize_t)
   with
     Global => (In_Out   => (Errors.Error_State, Contents),
                Proof_In => (FD_Table)),
     Post =>
     (case Has_Read is
        when -1                =>
          Contents = Contents'Old
            and then (if Errors.Get_Errno = Errors.ADA_EINTR
                      then Contains (Contents, Fd)),
        when 0                 =>
          Contents = Contents'Old
            and then Contains (Contents, Fd),
        when 1 .. ssize_t'Last =>
          Natural (Has_Read) <= Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Valid_Scalars
            and then
          Contains (Contents, Fd)
            and then
          M.Same_Keys (Model (Contents), Model (Contents'Old))
            and then
          Element (Contents, Fd).String
          = Append (Element (Contents'Old, Fd).String, Buf, Has_Read)
            and then
          M.Elements_Equal_Except (Model (Contents), Model (Contents'Old), Fd),
        when others          => False);

   procedure Write
     (Fd          : int;
      Buf         : in Init_String;
      Num_Bytes   : size_t;
      Has_Written : out ssize_t)
   with
     Global => (In_Out => (Errors.Error_State, Contents)),
     Pre    =>
       (Num_Bytes <= size_t (Integer'Last)
          and then Integer (Num_Bytes) <= Buf'Length
          and then Num_Bytes > 0
          and then
        Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars),
     Post =>
       (case Has_Written is
          when -1                =>
            Contents = Contents'Old
              and then (if Errors.Get_Errno = Errors.ADA_EINTR
                        then Contains (Contents, Fd)),
          when 0                 =>
            Contents = Contents'Old
              and then Contains (Contents, Fd),
          when 1 .. ssize_t'Last =>
            size_t (Has_Written) <= Num_Bytes
              and then
            Contains (Contents, Fd)
              and then
            M.Same_Keys (Model (Contents), Model (Contents'Old))
              and then
            Element (Contents, Fd).String
            = Append (Element (Contents'Old, Fd).String, Buf, Has_Written)
              and then
            M.Elements_Equal_Except (Model (Contents),
                                     Model (Contents'Old),
                                     Fd),
          when others            => False);

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
