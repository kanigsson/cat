with Ada.Containers; use Ada.Containers;
with Lemmas;         use Lemmas;

with Safe_Write;
procedure Full_Write
  (Fd        : int;
   Buf       : Init_String;
   Num_Bytes : size_t;
   Err       : out int)
with
  SPARK_Mode => On
is

   Has_Written : ssize_t := 0;
   --  Stores the total number of characters that have been written

   Has_Written_B : ssize_t;
   --  Stores the number of characters that have been written in the last
   --  call to Write.

   Num_Bytes_S : ssize_t := ssize_t (Num_Bytes);

   Contents_Old : Map (OPEN_MAX - 1, Default_Modulus (OPEN_MAX - 1)) :=
     Contents with Ghost;
   --  Updated at the beginning of each iteration of the loop to store the
   --  value of Contents at the end of last iteration.

   Contents_Pcd_Entry : constant Map (OPEN_MAX - 1,
                                      Default_Modulus (OPEN_MAX - 1)) :=
     Contents with Ghost;
   --  Stores the value of Contents at the beginning of the procedure

   --------------
   --  Lemmas  --
   --------------

   --  A property when calling Append with Bytes = 0
   procedure Equal_Implies_Append_Zero
   with
     Ghost,
     Pre =>
       Contains (Contents, Fd)
         and then Contains (Contents_Old, Fd)
         and then
       Element (Contents, Fd) = Element (Contents_Old, Fd)
         and then Has_Written_B = 0
         and then Integer (Has_Written) in 0 .. Buf'Length
         and then Buf'Last < Natural'Last,
     Post =>
       Element (Contents, Fd)
       = Append (Element (Contents_Old, Fd),
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B);
   procedure Equal_Implies_Append_Zero
   is null;

   --  Definition of "=" (L, R : Map).
   procedure Equal_Maps_Implies_Equal_Elements
   with
     Ghost,
     Pre =>
       Contents = Contents_Old
         and then Contains (Contents, Fd),
     Post => Element (Contents, Fd) = Element (Contents_Old, Fd);
   procedure Equal_Maps_Implies_Equal_Elements
   is null;

   --  Another lemma on Elements_Equal_Except.
   procedure Prove_Elements_Equal_Except
   with
     Ghost,
     Pre =>
       (M.Elements_Equal_Except
         (Model (Contents),
          Model (Contents_Old),
          Fd)
          or else
        Contents = Contents_Old)
          and then
       (M.Elements_Equal_Except
         (Model (Contents_Old),
          Model (Contents_Pcd_Entry),
          Fd)
          or else
        Contents_Old = Contents_Pcd_Entry),
      Post =>
        M.Elements_Equal_Except
          (Model (Contents),
           Model (Contents_Pcd_Entry),
           Fd);
   procedure Prove_Elements_Equal_Except
   is null;

begin

   while Has_Written /= Num_Bytes_S loop
      Contents_Old := Contents;
      --  Value of Contents at the beginning of the loop is stored
      --  in Contents_Old.

      pragma Assert (M.Elements_Equal_Except
                       (Model (Contents),
                        Model (Contents_Pcd_Entry),
                        Fd));

      pragma Assert (Buf (Buf'First + Integer (Has_Written)
                          ..
                          Buf'First + Integer (Has_Written) - 1
                          + Natural (Num_Bytes - size_t (Has_Written)))
                     = Buf (Buf'First + Integer (Has_Written)
                            ..
                            Buf'First - 1 + Natural (Num_Bytes)));
      --  A small assertion on equality of slices

      pragma Assert
        (if Contains (Contents, Fd)
         then Element (Contents_Old, Fd)
              = Append (Element (Contents_Pcd_Entry, Fd),
                        Buf,
                        Has_Written));
      --  This is a loop invariant, where Contents = Contents_Old

      Safe_Write (Fd,
             Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
             Num_Bytes - size_t (Has_Written),
             Has_Written_B);
      --  Data is written to Stdout

      Prove_Elements_Equal_Except;
      --  Necessary to prove the loop invariant about Elements_Equal_Except

      if Has_Written_B = -1 then
         Err := -1;
         return;
      end if;
      --  If an error occured, return

      pragma Assert (Contains (Contents, Fd));
      --  No error occured, then it means that the file is open

      if Has_Written_B = 0 then
         Equal_Maps_Implies_Equal_Elements;
         Equal_Implies_Append_Zero;
      end if;
      --  Necessary to prove a precondition of Prove_Full_Write_LI when
      --  Has_Written_B = 0.

      Prove_Full_Write_LI
        (Contents, Contents_Old, Contents_Pcd_Entry,
         Buf,
         Has_Written, Has_Written_B, Num_Bytes_S,
         Fd);
      --  Prove the last loop invariant

      Has_Written := Has_Written + Has_Written_B;
      --  The number of characters that have been written is
      --  Has_Written + Has_Written_B.

      pragma Assert (Natural (Has_Written) <= Natural (Num_Bytes));
      pragma Assert
        (Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars);
      pragma Assert
        (for all J in Buf'First .. Buf'First - 1 + Natural (Has_Written) =>
           (J in Buf'First .. Buf'First - 1 + Natural (Num_Bytes)
              and then Buf (J)'Valid_Scalars));
      pragma Assert
        (Buf (Buf'First
               ..
              Buf'First - 1 + Natural (Has_Written))'Valid_Scalars);
      --  Necessary to prove preconditions of loop invariants.

      pragma Loop_Invariant (M.Elements_Equal_Except
                              (Model (Contents),
                               Model (Contents_Pcd_Entry),
                               Fd));
      pragma Loop_Invariant (M.Same_Keys
                               (Model (Contents_Pcd_Entry),
                                Model (Contents)));
      --  Content is the same as before the call except for the content in Fd

      pragma Loop_Invariant (Has_Written in 0 .. Num_Bytes_S);
      --  We wrote between 0 and Num_Bytes_S characters

      pragma Loop_Invariant (Contains (Contents, Fd));
      --  The file is open

      pragma Loop_Invariant
        (Element (Contents, Fd)
         = Append (Element (Contents_Pcd_Entry, Fd), Buf, Has_Written));
      --  Property about the content in Fd.
   end loop;

   Err := 0;
end Full_Write;
