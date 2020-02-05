with Ada.Containers;      use Ada.Containers;
with Contents_Table_Type; use Contents_Table_Type;
with Interfaces.C;        use Interfaces.C;
with Iostr;               use Iostr;
with Errors;
use Contents_Table_Type.Maps;

package Stdio with
  SPARK_Mode        => On,
  Abstract_State    => (FD_Table),
  Initializes       => (FD_Table,
                        Contents),
  Initial_Condition =>
    (Has_Key (Contents, Stdin)
       and then Has_Key (Contents, Stdout)
       and then Has_Key (Contents, Stderr)
       and then One_String'(Get (Contents, Stdin))'Length = 0
       and then One_String'(Get (Contents, Stdout))'Length = 0
       and then One_String'(Get (Contents, Stderr))'Length = 0)
is

   pragma Elaborate_Body;

   OPEN_MAX : constant Count_Type := 1024;

   Contents : Map with Ghost;

   subtype off_t is int;

   procedure Open (File : char_array; Flags : int; Fd : out int) with
     Global => (In_Out => (FD_Table, Errors.Error_State, Contents)),
     Post   =>
       (case Fd is
          when -1                      => Contents'Old = Contents,
          when 0 .. int (OPEN_MAX - 1) =>
            Length (Contents'Old) + 1 = Length (Contents)
              and then
            Has_Key (Contents, Fd)
              and then
            One_String'(Get (Contents, Fd))'Length = 0
              and then
            not Has_Key (Contents'Old, Fd)
              and then
            Contents'Old <= Contents
              and then
            Keys_Included_Except (Contents,
                                    Contents'Old,
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
          not Has_Key (Contents, Fd)
            and then
        Contents <= Contents'Old
            and then
          Keys_Included_Except (Contents'Old, Contents, Fd),
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
                      then Has_Key (Contents, Fd)),
        when 0                 =>
          Contents = Contents'Old
            and then Has_Key (Contents, Fd),
        when 1 .. ssize_t'Last =>
          Natural (Has_Read) <= Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Positive (Has_Read))'Valid_Scalars
            and then
          Has_Key (Contents, Fd)
            and then
          Same_Keys (Contents, Contents'Old)
            and then
        Is_Append (Get (Contents'Old, Fd), One_String (Buf),
                   Get (Contents, Fd), Has_Read)
            and then
          Elements_Equal_Except (Contents, Contents'Old, Fd),
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
                        then Has_Key (Contents, Fd)),
          when 0                 =>
            Contents = Contents'Old
              and then Has_Key (Contents, Fd),
          when 1 .. ssize_t'Last =>
            size_t (Has_Written) <= Num_Bytes
              and then
          Has_Key (Contents, Fd)
              and then
            Same_Keys (Contents, Contents'Old)
              and then
          Is_Append (Get (Contents'Old, Fd),
                     One_String (Buf),
                     Get (Contents, Fd),
                       Has_Written)
              and then
            Elements_Equal_Except (Contents,
                                     Contents'Old,
                                     Fd),
          when others            => False);

   procedure Reset (Fd : int) with
     Ghost,
     Global => (In_Out => Contents),
     Post   =>
       (if not Has_Key (Contents'Old, Fd)
          then Contents'Old = Contents
        else
          Same_Keys (Contents, Contents'Old)
            and then
          Elements_Equal_Except (Contents, Contents'Old, Fd)
            and then
          One_String'(Get (Contents, Fd))'Length = 0);


   Stdin  : constant int := 0;
   Stdout : constant int := 1;
   Stderr : constant int := 2;

end Stdio;
