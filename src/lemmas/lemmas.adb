package body Lemmas with
SPARK_Mode => On
is

   --  In this package, the comment "Prove preconditions" is often present in
   --  preconditions of a procedure. This means that the preconditions above
   --  the comment are used to prove checks (preconditions, range checks)
   --  on the precondition lines below it.

   -----------------------------
   --  Lemmas on String type  --
   -----------------------------

   procedure Equality_Transitivity (A, B, C, D : String) with
     Pre =>
       C'First = 1
         and then D'Length <= Natural'Last - C'Length
         --  Prove preconditions

         and then A = B
         and then A = C & D,
       Post => B = C & D;
   procedure Equality_Transitivity (A, B, C, D : String) is null;

   procedure Left_Substitution_In_Concat (Str, L_1, L_2, R : String) with
     Pre =>
       L_1'Length <= Natural'Last - R'Length
         and then L_1'First = 1
         and then L_2'First = 1
      --  Prove preconditions

         and then L_1 = L_2
         and then Str = L_1 & R,
       Post => Str = L_2 & R;
   procedure Left_Substitution_In_Concat (Str, L_1, L_2, R : String) is null;

   procedure Left_Substitution_In_Concat_2 (A, B, C, D, E : String) with
     Pre =>
       B'First = 1
         and then D'First = 1
         and then E'First = 1
         and then D'Length <= Natural'Last - E'Length
         and then C'Length <= Natural'Last - B'Length
         --  Prove preconditions

         and then A = B & C
         and then B = D & E,
       Post => A = D & E & C;
   procedure Left_Substitution_In_Concat_2 (A, B, C, D, E : String) is null;

   procedure No_Priority_On_Concat (A, B, C, D : String) with
     Pre =>
       B'First = 1
         and then C'First = 1
         and then D'Length <= Natural'Last - B'Length - C'Length
         --  Prove preconditions

         and then A = B & C & D,
       Post => A = B & (C & D);
   procedure No_Priority_On_Concat (A, B, C, D : String) is null;

   procedure Right_Substitution_In_Concat (Str, L, R_1, R_2 : String) with
     Pre =>
       R_1'Length <= Natural'Last - L'Length
         and then L'First = 1
         --  Prove preconditions

         and then R_1 = R_2
         and then Str = L & R_1,
       Post => Str = L & R_2;
   procedure Right_Substitution_In_Concat (Str, L, R_1, R_2 : String) is null;

   ----------------------
   --  Helpful lemmas  --
   ----------------------

   --------------------------------------------
   --  To_String_Distributivity_Over_Concat  --
   --------------------------------------------

   procedure To_String_Distributivity_Over_Concat (Buf, L, R : Init_String)
   with
     Pre =>
       Buf'Valid_Scalars
         and then L'Valid_Scalars
         and then R'Valid_Scalars
         and then L'Length <= Natural'Last - R'Length
         and then L'Last <= Natural'Last - R'Length
         --  Prove preconditions

         and then Buf = L & R,
     Post => To_String (Buf) = To_String (L) & To_String (R);
   procedure To_String_Distributivity_Over_Concat (Buf, L, R : Init_String)
   is null;

   ----------------------------
   --  Append_Result_Length  --
   ----------------------------

   --  Gives the length of the result of a call to Append depending on
   --  the parameters.
   procedure Append_Result_Length
     (A : Unbounded_String;
      Buf : Init_String;
      Num_Bytes : ssize_t)
   with
     Pre =>
       Num_Bytes in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars,
     Post =>
       (if Num_Bytes = 0
          then Length (Append (A, Buf, Num_Bytes)) = Length (A)
        --  First case

        elsif Natural (Num_Bytes) <= Natural'Last - Length (A)
          then Length (Append (A, Buf, Num_Bytes))
               = Length (A) + Natural (Num_Bytes)
        --  Second case

        else Length (Append (A, Buf, Num_Bytes)) = Natural'Last);
        --  Third case
   procedure Append_Result_Length
     (A : Unbounded_String;
      Buf : Init_String;
      Num_Bytes : ssize_t)
   is
   begin
      if Length (A) = Natural'Last or else Num_Bytes = 0 then
         pragma Assert (Length (Append (A, Buf, Num_Bytes)) = Length (A));
      else
         pragma Assert (Length (Append (A, Buf, Num_Bytes)) >= Length (A));
      end if;
      --  Case-splitting above. It is easier to handle for provers.
   end Append_Result_Length;

   ------------------------------------
   --  Reverse_Append_Result_Length  --
   ------------------------------------

   --  Gives properties about the parameters of Append depending on the
   --  Length of its result.
   procedure Reverse_Append_Result_Length
     (A : Unbounded_String;
      Buf : Init_String;
      Num_Bytes : ssize_t)
   with
     Pre =>
       Num_Bytes in 0 .. Buf'Length
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes))'Valid_Scalars,
     Post =>
       (if Length (Append (A, Buf, Num_Bytes)) = Natural'Last
        then (Natural (Num_Bytes) >= Natural'Last - Length (A))
        --  If the Length is maximal

        else Natural (Num_Bytes) < Natural'Last - Length (A));
        --  Other case
   procedure Reverse_Append_Result_Length
     (A : Unbounded_String;
      Buf : Init_String;
      Num_Bytes : ssize_t) is null;

   ---------------------------
   --  Body of public part  --
   ---------------------------

   -----------------------------------
   --  Left_Substitution_In_Concat  --
   -----------------------------------

   procedure Left_Substitution_In_Concat
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : int)
   is
   begin
      pragma Assert (To_String (Left_1) = To_String (Left_2));
      --  Left_1 = Left_2 so it is true when converted to Strings

      if Natural (Bytes) <= Natural'Last - Length (Left_1) then
      --  First case: Bytes characters can be appended to Left_1

         Left_Substitution_In_Concat
           (To_String (Str),
            To_String (Left_1),
            To_String (Left_2),
            To_String (Buf (Buf'First .. Buf'First - 1 + Natural (Bytes))));
         --  Translating the problem into a Strings problem

      else
      --  Second case: Bytes characters can't be appended to Left_1

         declare
            New_Bytes : constant Natural := Natural'Last - Length (Left_1);
            --  Number of bytes that we can actually write

         begin
            Left_Substitution_In_Concat (To_String (Str),
                                         To_String (Left_1),
                                         To_String (Left_2),
                                         To_String
                                           (Buf (Buf'First
                                                  ..
                                                 Buf'First - 1
                                                 + Natural (New_Bytes))));
            --  Translating the problem into a Strings problem

            pragma Assert
              (To_String (Left_1)
               & To_String (Buf (Buf'First .. Buf'First - 1 + New_Bytes))
               = To_String (Left_2)
               & To_String (Buf (Buf'First .. Buf'First - 1 + New_Bytes)));
            --  Postcondition of previous procedure

            pragma Assert
              (Append (Left_1, Buf, Bytes) = Append (Left_2, Buf, Bytes));
            --  Postcondition of current procedure
         end;
      end if;
   end Left_Substitution_In_Concat;

   -------------------------------
   --  Prove_Copy_To_Stdout_LI  --
   -------------------------------

   procedure Prove_Copy_To_Stdout_LI
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Read                                   : ssize_t;
      Input, Stdout                              : int)
   is

      --------------------------
      --  First_Substitution  --
      --------------------------

      procedure First_Substitution
        (A, B, C, D : Unbounded_String;
         Buf        : Init_String;
         Has_Read   : ssize_t)
      with
        Pre =>
          Has_Read in 0 .. Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
          --  Prove preconditions

            and then A = Append (B, Buf, Has_Read)
            and then B = C & D,
        Post => A = Append (C & D, Buf, Has_Read);
      --  We can substitute B by C & D in the call to Append

      ------------------------------
      --  Taking_B_Out_Of_Append  --
      ------------------------------

      procedure Taking_B_Out_Of_Append
        (A, B, C  : Unbounded_String;
         Buf      : Init_String;
         Has_Read : ssize_t)
      with
        Pre =>
          Has_Read in 0 .. Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
          --  Prove preconditions

            and then A = Append (B & C, Buf, Has_Read),
        Post => A = B & Append (C, Buf, Has_Read);
        --  We can take B out of the call to Append

      procedure Second_Substitution
        (A, B, C, D : Unbounded_String;
         Buf        : Init_String;
         Has_Read   : ssize_t)
      with
        Pre =>
          Has_Read in 0 .. Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
          --  Prove preconditions

            and then A = B & Append (C, Buf, Has_Read)
            and then Append (C, Buf, Has_Read) = D,
        Post => A = B & D;
      --  We can substitute the call to Append by its value D.

      procedure Second_Substitution
        (A, B, C, D : Unbounded_String;
         Buf        : Init_String;
         Has_Read   : ssize_t)
      is null;

      procedure First_Substitution
        (A, B, C, D : Unbounded_String;
         Buf        : Init_String;
         Has_Read   : ssize_t)
      is
      begin

         if Length (B) = Natural'Last or else Has_Read = 0 then
         --  First case in postcondition of Append

            pragma Assert (A = Append (C & D, Buf, Has_Read));

         elsif Natural (Has_Read) <= Natural'Last - Length (B) then
         --  Second case in postcondition of Append

            Left_Substitution_In_Concat
              (To_String (A),
               To_String (B),
               To_String (C & D),
               To_String
                 (Buf (Buf'First
                        ..
                       Buf'First - 1 + Natural (Has_Read))));
            --  Translation to a Strings problem

            pragma Assert (A = Append (C & D, Buf, Has_Read));

         else
         --  Third case in postcondition of Append

            pragma Assert
              (To_String (A)
               = To_String (B)
               & To_String (Buf (Buf'First
                 ..
                   Natural'Last - Length (B) - 1 + Buf'First)));
            Left_Substitution_In_Concat
              (To_String (A),
               To_String (B),
               To_String (C & D),
               To_String (Buf (Buf'First
                                ..
                               Natural'Last - Length (B) - 1 + Buf'First)));
            --  Translation to a Strings problem

            pragma Assert (Length (B) = Length (C & D));
            pragma Assert (Natural (Has_Read) > Natural'Last - Length (C & D));
            --  To determine which case it is in postcondition of Append

            pragma Assert (A = Append (C & D, Buf, Has_Read));
         end if;
      end First_Substitution;

      procedure Taking_B_Out_Of_Append
        (A, B, C  : Unbounded_String;
         Buf      : Init_String;
         Has_Read : ssize_t)
      is

         --  The proof of Taking_B_Out_Of_Append is splitted into 4 cases.

         ---------------------
         --  Null_Has_Read  --
         ---------------------

         --  First case, Has_Read = 0
         procedure Null_Has_Read with
           Pre =>
             Has_Read = 0
               and then
             A = Append (B & C, Buf, Has_Read),
           Post => A = B & Append (C, Buf, Has_Read);

         procedure Null_Has_Read is
            --  Lemma about output of Append when Num_Bytes = 0
            procedure Lemma
              (String    : Unbounded_String;
               Buffer    : Init_String;
               Num_Bytes : ssize_t)
            with
              Pre => Num_Bytes = 0,
              Post => Append (String, Buffer, Num_Bytes) = String;
            procedure Lemma
              (String    : Unbounded_String;
               Buffer    : Init_String;
               Num_Bytes : ssize_t) is null;
         begin
            Lemma (B & C, Buf, Has_Read);
            --  Append (B & C, Buf, 0) = B & C

            Equality_Transitivity (A, Append (B & C, Buf, Has_Read), B, C);
            --  A = B & C

            Lemma (C, Buf, Has_Read);
            --  C = Append (C, Buf, 0)

            Right_Substitution_In_Concat (A, B, C, Append (C, Buf, Has_Read));
            --  A = B & Append (C, Buf, 0)
         end Null_Has_Read;

         ---------------------------------
         --  Append_B_C_Has_Max_Length  --
         ---------------------------------

         --  Second case: Has_Read /= 0 and Length (B & C) = Natural'Last

         procedure Append_B_C_Has_Max_Length with
           Pre =>
             Has_Read in 1 .. Buf'Length
               and then
             Buf (Buf'First
                  ..
                  Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
                  --  Prove preconditions

               and then A = Append (B & C, Buf, Has_Read)
               and then Length (B & C) = Natural'Last,
           Post => A = B & Append (C, Buf, Has_Read);

         procedure Append_B_C_Has_Max_Length is
         begin

            pragma Assert (Length (C) >= Natural'Last - Length (B));
            pragma Assert
              (To_String (Append (B & C, Buf, Has_Read))
               = To_String (B & C));
            pragma Assert (To_String (A) = To_String (B & C));
            pragma Assert (To_String (A)
                           = To_String (B)
                           & To_String (C) (1 .. Natural'Last - Length (B)));
            --  C has to be cut when appended to B

            Append_Result_Length (C, Buf, Has_Read);
            --  Instantiate the lemma to reason in the following block

            if Length (C) = Natural'Last then
               pragma Assert (To_String (Append (C, Buf, Has_Read))
                              (1 .. Natural'Last - Length (B))
                              = To_String (C)
                                  (1 .. Natural'Last - Length (B)));

            elsif Natural (Has_Read) <= Natural'Last - Length (C) then
               pragma Assert (To_String (Append (C, Buf, Has_Read))
                              (1 .. Natural'Last - Length (B))
                              = To_String (C)
                                  (1 .. Natural'Last - Length (B)));

            else
               pragma Assert (To_String (Append (C, Buf, Has_Read))
                              (1 .. Natural'Last - Length (B))
                              = To_String (C)
                                  (1 .. Natural'Last - Length (B)));
            end if;
            --  The previous block proves that for any value of Has_Read,
            --  the slice containing the first Natural'Last - Length (B)
            --  characters of Append (C, Buf, Has_Read) is a slice of C.

            pragma Assert (A = B & Append (C, Buf, Has_Read));
         end Append_B_C_Has_Max_Length;

         ---------------------------------
         --  Has_Read_Less_Than_Length  --
         ---------------------------------

         --  Third case: Has_Read /= 0 and Length (B & C) /= Natural'Last
         --  and Has_Read characters can be appended to B & C.

         procedure Has_Read_Less_Than_Length with
           Pre =>
             Has_Read in 1 .. Buf'Length
               and then
             Buf (Buf'First
                   ..
                  Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
                  --  Prove preconditions

               and then A = Append (B & C, Buf, Has_Read)
               and then Natural (Has_Read) <= Natural'Last - Length (B & C)
               and then Natural'Last /= Length (B & C),
           Post => A = B & Append (C, Buf, Has_Read);
         procedure Has_Read_Less_Than_Length is
         begin
            Left_Substitution_In_Concat
              (To_String (A),
               To_String (B & C),
               To_String (B) & To_String (C),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Read))));
            --  We just need to translate the problem in a Strings problem
            No_Priority_On_Concat (To_String (A),
                                   To_String (B),
                                   To_String (C),
                                   To_String (Buf (Buf'First
                                     ..
                                       Buf'First - 1 + Natural (Has_Read))));
            --  We can group the two last strings together
            --  (To_String (C) & To_String (Buf (Buf'First .. Buf'First - 1
            --                                                + Has_Read))
            --   = To_String (Append (C, Buf, Has_Read)))
            Right_Substitution_In_Concat
              (To_String (A),
               To_String (B),
               To_String (C)
               & To_String
                   (Buf (Buf'First
                          ..
                         Buf'First - 1 + Natural (Has_Read))),
               To_String (Append (C, Buf, Has_Read)));
            --  Substituing the equality in the comment above

            pragma Assert
              (To_String (A)
               = To_String (B)
               & To_String (Append (C, Buf, Has_Read)));
            --  Postcondition translated to Strings

            pragma Assert (A = B & Append (C, Buf, Has_Read));
         end Has_Read_Less_Than_Length;

         ------------------------------------
         --  Has_Read_Greater_Than_Length  --
         ------------------------------------

         --  Fourth and last case

         procedure Has_Read_Greater_Than_Length with
           Pre =>
             Has_Read in 1 .. Buf'Length
               and then
               Buf (Buf'First
                    ..
                    Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
                    --  Prove preconditions

               and then A = Append (B & C, Buf, Has_Read)
               and then Natural'Last /= Length (B & C)
               and then Natural (Has_Read) > Natural'Last - Length (B & C),
           Post => A = B & Append (C, Buf, Has_Read);
         procedure Has_Read_Greater_Than_Length is
         begin
            Left_Substitution_In_Concat
              (To_String (A),
               To_String (B & C),
               To_String (B) & To_String (C),
               To_String (Buf (Buf'First
                               ..
                               Natural'Last - Length (B) - Length (C) - 1
                               + Buf'First)));
            --  Translating to a Strings problem

            No_Priority_On_Concat
              (To_String (A),
               To_String (B),
               To_String (C),
               To_String (Buf (Buf'First
                                ..
                               Natural'Last - Length (B) - Length (C) - 1
                               + Buf'First)));
            --  We can group the two last strings together

            pragma Assert (Length (B & C) /= Natural'Last
                           and then Length (B & C) = Length (B) + Length (C));
            pragma Assert
              (Natural (Has_Read)
               > Natural'Last - Length (B) - Length (C));
            --  Precondition of this procedure

            if Natural (Has_Read) <= Natural'Last - Length (C) then
               pragma Assert
                 (Length (Append (C, Buf, Has_Read))
                  >= Natural'Last - Length (B));
            else
               pragma Assert
                 (Length (Append (C, Buf, Has_Read))
                  >= Natural'Last - Length (B));
            end if;
            --  Case splitting to prove that Length (Append (C, Buf, Has_Read))
            --  is greater than Natural'Last - Length (B) no matter the value
            --  of Has_Read.

            pragma Assert
              (To_String (C)
               & To_String (Buf (Buf'First
                 ..
                   Natural'Last - Length (B) - Length (C) - 1
                 + Buf'First))
               = To_String (Append (C, Buf, Has_Read))
               (1 .. Natural'Last - Length (B)));
            --  The slice of the first Natural'Last - Length (B) characters of
            --  Append (C, ...) is a slice of C.

            pragma Assert
              (Length (Append (C, Buf, Has_Read))
               >= Natural'Last - Length (B));
            --  To determine the case in postcondition to
            --  B & Append (C, Buf, Has_Read)

            Right_Substitution_In_Concat
              (To_String (A),
               To_String (B),
               To_String (C)
               & To_String (Buf (Buf'First
                                 ..
                                 Natural'Last - Length (B) - Length (C) - 1
                                 + Buf'First)),
              To_String (Append (C, Buf, Has_Read))
                (1 .. Natural'Last - Length (B)));
            --  Substitution of Append (C, Buf, Has_Read) by the value in
            --  postcondition

            pragma Assert
              (To_String (A)
               = To_String (B)
               & To_String (Append (C, Buf, Has_Read))
               (1 .. Natural'Last - Length (B)));
            --  Postcondition of this procedure translated to Strings

            pragma Assert (A = B & Append (C, Buf, Has_Read));
         end Has_Read_Greater_Than_Length;
      begin

         if Has_Read = 0 then
            Null_Has_Read;
         elsif Length (B & C) = Natural'Last then
            Append_B_C_Has_Max_Length;
         elsif Natural (Has_Read) <= Natural'Last - Length (B & C) then
            Has_Read_Less_Than_Length;
         else
            Has_Read_Greater_Than_Length;
         end if;
         --  Each procedure is called with the right preconditions.

      end Taking_B_Out_Of_Append;

   begin
      First_Substitution (Element (Contents, Stdout),
                          Element (Contents_Old, Stdout),
                          Element (Contents_Pcd_Entry, Stdout),
                          Element (Contents_Old, Input),
                          Buf,
                          Has_Read);
      --  Element (Contents, Stdout)
      --  = Append (Element (Contents_Old, Stdout), Buf, Has_Read)
      --  = Append (Element (Contents_Pcd_Entry, Stdout)
      --            & Element (Contents_Old, Input),
      --            Buf,
      --            Has_Read)

      Taking_B_Out_Of_Append (Element (Contents, Stdout),
                              Element (Contents_Pcd_Entry, Stdout),
                              Element (Contents_Old, Input),
                              Buf,
                              Has_Read);
      --  Append (Element (Contents_Pcd_Entry, Stdout)
      --          & Element (Contents_Old, Input),
      --          Buf,
      --          Has_Read)
      --  = Element (Contents_Pcd_Entry, Stdout)
      --  & Append (Element (Contents_Old, Input), Buf, Has_Read)

      Second_Substitution (Element (Contents, Stdout),
                           Element (Contents_Pcd_Entry, Stdout),
                           Element (Contents_Old, Input),
                           Element (Contents, Input),
                           Buf,
                           Has_Read);
      --  Element (Contents_Pcd_Entry, Stdout)
      --  & Append (Element (Contents_Old, Input), Buf, Has_Read)
      --  = Element (Contents_Pcd_Entry, Stdout) & Element (Contents, Input)
      --  (= Element (Contents, Stdout))

   end Prove_Copy_To_Stdout_LI;

   ----------------------------
   --  Prove_Full_Write_LI  --
   ----------------------------

   procedure Prove_Full_Write_LI
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S    : ssize_t;
      Fd                                         : int)
   is

      -------------------------------------------
      --  Prove_Postcondition_String_Version  --
      -------------------------------------------

      --  Postcondition of Prove_Full_Write_LI with
      --  Unbounded_String parameters to avoid context size growth because
      --  of Maps theories.

      procedure Prove_Postcondition_String_Version
        (String, Old_String, Pcd_Entry_String : Unbounded_String)
      with
        Pre =>
          Num_Bytes_S in 1 .. ssize_t (Integer'Last)
            and then Integer (Num_Bytes_S) <= Buf'Length
            and then Buf'Last < Natural'Last
            and then Has_Written in 0 .. Num_Bytes_S
            and then Has_Written_B in 0 .. Num_Bytes_S - Has_Written
            and then
          Buf (Buf'First
               ..
               Buf'First - 1 + Natural (Num_Bytes_S))'Valid_Scalars
               --  Prove preconditions

            and then
          Old_String
          = Append (Pcd_Entry_String, Buf, Has_Written)
            and then
          String
          = Append (Old_String,
                    Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                    Has_Written_B),
        Post =>
          String = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B);

      procedure Prove_Postcondition_String_Version
        (String, Old_String, Pcd_Entry_String : Unbounded_String)
      is

         ------------------------------------------------
         --  From_String_To_Unbounded_String_Property  --
         ------------------------------------------------

         --  This procedures allows to go from properties on Strings
         --  to properties on Unbounded_Strings.

         procedure From_String_To_Unbounded_String_Property
         with
           Pre =>
             Num_Bytes_S in 1 .. ssize_t (Integer'Last)
               and then Integer (Num_Bytes_S) <= Buf'Length
               and then Buf'Last < Natural'Last
               and then Has_Written in 0 .. Num_Bytes_S
               and then Has_Written_B in 0 .. Num_Bytes_S - Has_Written
               and then
             Buf (Buf'First
                  ..
                  Buf'First - 1 + Natural (Num_Bytes_S))'Valid_Scalars
                  --  Prove preconditions

               and then
             (if Integer (Has_Written + Has_Written_B)
                 <= Natural'Last - Length (Pcd_Entry_String)
              then
                To_String (String)
                = To_String (Pcd_Entry_String)
                & To_String
                  (Buf (Buf'First
                        ..
                        Buf'First - 1
                        + Integer (Has_Written + Has_Written_B)))
              else
                (Length (Old_String) - Integer (Has_Written)
                 = Length (Pcd_Entry_String)
                   and then
                 To_String (String)
                 = To_String (Pcd_Entry_String)
                 & To_String
                     (Buf (Buf'First
                            ..
                           Natural'Last - Length (Old_String) - 1 + Buf'First
                           + Integer (Has_Written))))),
             --  Precondition above states the value of To_String (String)
             --  depending on the value of Has_Written + Has_Written_B.

           Post =>
             String = Append (Pcd_Entry_String,
                              Buf,
                              Has_Written + Has_Written_B);
         procedure From_String_To_Unbounded_String_Property
         is
         begin
            if Integer (Has_Written + Has_Written_B)
               > Natural'Last - Length (Pcd_Entry_String)
            then
            --  Case-splitting on the value of Has_Written + Has_Written_B

               pragma Assert
                 (Length (Old_String) - Integer (Has_Written)
                  = Length (Pcd_Entry_String));
               --  This is a precondition

               pragma Assert
                 (To_String (String)
                  = To_String (Pcd_Entry_String)
                  & To_String
                    (Buf (Buf'First
                          ..
                          Natural'Last - Length (Pcd_Entry_String) - 1
                          + Buf'First)));
               --  Substitution of Length (Old_String) - Integer (Has_Written)
               --  in precondition by Length (Pcd_Entry_String).

               pragma Assert
                 (String
                  = Append (Pcd_Entry_String,
                            Buf,
                            Has_Written + Has_Written_B));
               --  Postcondition to prove

            else
               pragma Assert
                 (String
                  = Append (Pcd_Entry_String,
                            Buf,
                            Has_Written + Has_Written_B));
               --  Postcondition to prove. This case is easy for the provers.

            end if;
         end From_String_To_Unbounded_String_Property;

      begin
         pragma Assert
           (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))
            & Buf (Buf'First + Integer (Has_Written) .. Buf'Last)
                (Buf'First + Integer (Has_Written)
                 ..
                 Buf'First - 1 + Integer (Has_Written)
                 + Integer (Has_Written_B))
            = Buf (Buf'First
                    ..
                   Buf'First - 1 + Integer (Has_Written)
                   + Integer (Has_Written_B)));
         --  An equality on slices and concatenation.

         if Has_Written_B = 0 then
         --  Case-splitting: case where Has_Written_B = 0

            pragma Assert
              (String
               = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
            --  Easy because nothing has been appended.

         elsif Length (Old_String) = Natural'Last then
         --  Second case: Old_String has maximal length

            pragma Assert
              (To_String (String) = To_String (Old_String));
            --  String = Append (Old_String, ...) and
            --  Length (Old_String) = Natural'Last

            pragma Assert
              (To_String (Old_String)
               = To_String (Append (Old_String,
                 Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                 Has_Written_B)));
            --  Old_String has maximal length so when converted to string,
            --  any concatenation translated to String has the same value.

            pragma Assert (To_String (String) = To_String (Old_String));

            pragma Assert
              (To_String (Old_String)
               = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));
            --  This is a precondition

            if Length (Pcd_Entry_String) = Natural'Last then
            --  Second case-splitting on the length of Pcd_Entry_String

               pragma Assert
                 (To_String (Append (Pcd_Entry_String,
                  Buf,
                  Has_Written + Has_Written_B))
                  = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));
               --  Easy because Pcd_Entry_String has maximal length

            else
               Reverse_Append_Result_Length
                 (Pcd_Entry_String, Buf, Has_Written);
               pragma Assert
                 (Natural (Has_Written)
                  >= Natural'Last - Length (Pcd_Entry_String));
               --  Append (Pcd_Entry_String) has maximal length
               --  and Pcd_Entry_String has not, so this is true.

               if Natural (Has_Written)
                  = Natural'Last - Length (Pcd_Entry_String)
               then
               --  Third case-splitting
                  pragma Assert
                    (To_String (Append (Pcd_Entry_String,
                     Buf,
                     Has_Written + Has_Written_B))
                     = To_String
                         (Append (Pcd_Entry_String, Buf, Has_Written)));
               else
                  pragma Assert
                    (To_String (Append (Pcd_Entry_String,
                     Buf,
                     Has_Written + Has_Written_B))
                     = To_String
                         (Append (Pcd_Entry_String, Buf, Has_Written)));
               end if;
            end if;

            pragma Assert
              (String
               = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
            --  This property is easy to prove after the case-splittings above

         elsif Integer (Has_Written_B) <= Natural'Last - Length (Old_String)
         then
         --  Third case: Has_Written_B can be appended to Old_String

            pragma Assert
              (To_String (Append (Old_String,
               Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
               Has_Written_B))
               = To_String (Old_String)
               & To_String
                 (Buf (Buf'First + Integer (Has_Written)
                  ..
                    Buf'First + Integer (Has_Written) - 1
                  + Natural (Has_Written_B))));
            --  Translating to Strings

            Equality_Transitivity (To_String
                                    (Append (Old_String,
                                     Buf (Buf'First + Integer (Has_Written)
                                           ..
                                          Buf'Last),
                                     Has_Written_B)),
                                   To_String (String),
                                   To_String (Old_String),
                                   To_String
                                     (Buf (Buf'First + Integer (Has_Written)
                                      ..
                                        Buf'First + Integer (Has_Written) - 1
                                      + Natural (Has_Written_B))));
            --  Using transitivity to prove the equality for To_String (String)

            pragma Assert (Length (Old_String) < Natural'Last);
            pragma Assert
              (Length (Old_String)
               = Length (Append (Pcd_Entry_String, Buf, Has_Written)));
            Reverse_Append_Result_Length (Pcd_Entry_String, Buf, Has_Written);
            --  Instantiating the lemma before doing a case-splitting on the
            --  value of Has_Written

            if Has_Written = 0 then
               pragma Assert
                 (To_String (Append (Pcd_Entry_String, Buf, Has_Written))
                  = To_String (Pcd_Entry_String)
                  & To_String (Buf (Buf'First
                    ..
                      Buf'First - 1 + Natural (Has_Written))));
            else
               pragma Assert
                 (To_String (Append (Pcd_Entry_String, Buf, Has_Written))
                  = To_String (Pcd_Entry_String)
                  & To_String (Buf (Buf'First
                    ..
                      Buf'First - 1 + Natural (Has_Written))));

            end if;
            --  The properties above are the same but are easier to prove
            --  when doing a case splitting.

            Equality_Transitivity
              (To_String (Append (Pcd_Entry_String,
                                  Buf,
                                  Has_Written)),
               To_String (Old_String),
               To_String (Pcd_Entry_String),
               To_String
                 (Buf (Buf'First
                        ..
                       Buf'First - 1
                       + Natural (Has_Written))));
            --  Using again transitivity of equality

            Left_Substitution_In_Concat_2
              (To_String (String),
               To_String (Old_String),
               To_String
                 (Buf (Buf'First + Integer (Has_Written)
                       ..
                       Buf'First + Integer (Has_Written) - 1
                       + Natural (Has_Written_B))),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                          ..
                          Buf'First - 1 + Natural (Has_Written))));

            No_Priority_On_Concat (To_String (String),
                                   To_String (Pcd_Entry_String),
                                   To_String (Buf (Buf'First
                                     ..
                                       Buf'First - 1 + Natural (Has_Written))),
                                   To_String
                                     (Buf (Buf'First + Integer (Has_Written)
                                      ..
                                        Buf'First + Integer (Has_Written) - 1
                                      + Natural (Has_Written_B))));
            --  Substitution and grouping elements of concatenation

            pragma Assert
              (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))
               & Buf (Buf'First + Integer (Has_Written) .. Buf'Last)
                  (Buf'First + Integer (Has_Written)
                   ..
                   Buf'First - 1 + Integer (Has_Written)
                   + Integer (Has_Written_B))
               = Buf (Buf'First
                       ..
                      Buf'First - 1 + Integer (Has_Written)
                      + Integer (Has_Written_B)));
            --  This is the assertion proved at the beginning of the procedure.

            To_String_Distributivity_Over_Concat
              (Buf (Buf'First
                     ..
                    Buf'First - 1 + Integer (Has_Written)
                    + Integer (Has_Written_B)),
               Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written)),
               Buf (Buf'First + Integer (Has_Written)
                 ..
                   Buf'First + Integer (Has_Written) - 1
                 + Natural (Has_Written_B)));
            --  Using the distributivity of To_String function over
            --  over concatenation of Init_String variables. It helps proving
            --  a precondition of the following procedure.

            Right_Substitution_In_Concat
              (To_String (String),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Written)))
               & To_String
                   (Buf (Buf'First + Integer (Has_Written)
                         ..
                         Buf'First + Integer (Has_Written) - 1
                         + Natural (Has_Written_B))),
               To_String
                 (Buf (Buf'First
                       ..
                       Buf'First - 1 + Integer (Has_Written)
                       + Integer (Has_Written_B))));
            --  To_String (String)
            --  = To_String (Pcd_Entry_String)
            --  & To_String (Buf (Buf'First
            --                     ..
            --                    Buf'First - 1 + Integer (Has_Written)
            --                    + Integer (Has_Written_B)))

            pragma Assert
              (Integer (Has_Written) + Integer (Has_Written_B)
               <= Natural'Last - Length (Pcd_Entry_String));
            --  Helps to determine in which case we are in the call to Append

            pragma Assert
              (To_String (String)
               = To_String (Pcd_Entry_String)
               & To_String
                 (Buf (Buf'First
                  ..
                    Buf'First - 1 + Integer (Has_Written)
                  + Integer (Has_Written_B))));
            pragma Assert
              (Integer (Has_Written) + Integer (Has_Written_B)
               = Integer (Has_Written + Has_Written_B));

            pragma Assert
              (To_String (String)
               = To_String (Pcd_Entry_String)
               & To_String (Buf (Buf'First
                 ..
                   Buf'First - 1 + Integer (Has_Written + Has_Written_B))));
            --  The three asertions above helps proving the following.

            pragma Assert
              (String
               = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));

         else
         --  Fourth case: it is not possible to append Has_Written_B characters
         --  to Old_String. The process to prove this case is very similar
         --  to the third case, except that we replace
         --  the last index of the slice (Buf'First + Integer (Has_Written) - 1
         --                               + Natural (Has_Written_B))
         --  by (Natural'Last - Length (Old_String) - 1 + Buf'First
         --      + Integer (Has_Written)).

            pragma Assert
              (To_String (Append (Old_String,
               Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
               Has_Written_B))
               = To_String (Old_String)
               & To_String
                 (Buf (Buf'First + Integer (Has_Written)
                  ..
                    Natural'Last - Length (Old_String) - 1 + Buf'First
                  + Integer (Has_Written))));
            --  Translating to Strings

            Equality_Transitivity
              (To_String (Append (Old_String,
                                  Buf (Buf'First + Integer (Has_Written)
                                       ..
                                       Buf'Last),
                                  Has_Written_B)),
               To_String (String),
               To_String (Old_String),
               To_String
                 (Buf (Buf'First + Integer (Has_Written)
                       ..
                       Natural'Last - Length (Old_String) - 1 + Buf'First
                       + Integer (Has_Written))));
            --  Using transitivity to prove the equality for To_String (String)

            pragma Assert (Length (Old_String) < Natural'Last);
            pragma Assert
              (Length (Old_String)
               = Length (Append (Pcd_Entry_String, Buf, Has_Written)));
            Reverse_Append_Result_Length (Pcd_Entry_String, Buf, Has_Written);
            --  Instantiating the lemma before doing a case-splitting

            if Has_Written = 0 then
               pragma Assert
                 (To_String (Append (Pcd_Entry_String, Buf, Has_Written))
                  = To_String (Pcd_Entry_String)
                  & To_String (Buf (Buf'First
                    ..
                      Buf'First - 1 + Natural (Has_Written))));
            else
               pragma Assert
                 (To_String (Append (Pcd_Entry_String, Buf, Has_Written))
                  = To_String (Pcd_Entry_String)
                  & To_String (Buf (Buf'First
                    ..
                      Buf'First - 1 + Natural (Has_Written))));

            end if;
            --  Case-splitting helps the provers.

            Equality_Transitivity
              (To_String (Append (Pcd_Entry_String, Buf, Has_Written)),
               To_String (Old_String),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Written))));
            --  Using again transitivity of equality

            Left_Substitution_In_Concat_2
              (To_String (String),
               To_String (Old_String),
               To_String
                 (Buf (Buf'First + Integer (Has_Written)
                       ..
                       Natural'Last - Length (Old_String) - 1 + Buf'First
                       + Integer (Has_Written))),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Written))));

            No_Priority_On_Concat
              (To_String (String),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Written))),
               To_String
                 (Buf (Buf'First + Integer (Has_Written)
                        ..
                       Natural'Last - Length (Old_String) - 1 + Buf'First
                       + Integer (Has_Written))));
            --  Substitution and grouping elements of concatenation

            To_String_Distributivity_Over_Concat
              (Buf (Buf'First
               ..
                 Natural'Last - Length (Old_String) - 1 + Buf'First
               + Integer (Has_Written)),
               Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written)),
               Buf (Buf'First + Integer (Has_Written)
                 ..
                   Natural'Last - Length (Old_String) - 1 + Buf'First
                 + Integer (Has_Written)));
            --  This procedure is called on the assertion proved at the
            --  beginning of Prove_Postcondition_String_Version.

            Right_Substitution_In_Concat
              (To_String (String),
               To_String (Pcd_Entry_String),
               To_String (Buf (Buf'First
                               ..
                               Buf'First - 1 + Natural (Has_Written)))
               & To_String
                 (Buf (Buf'First + Integer (Has_Written)
                       ..
                       Natural'Last - Length (Old_String) - 1 + Buf'First
                       + Integer (Has_Written))),
               To_String
                 (Buf (Buf'First
                       ..
                       Natural'Last - Length (Old_String) - 1 + Buf'First
                       + Integer (Has_Written))));
            --  Substitution

            pragma Assert
              (To_String (String)
               = To_String (Pcd_Entry_String)
               & To_String
                 (Buf (Buf'First
                  ..
                    Natural'Last - Length (Old_String) - 1 + Buf'First
                  + Integer (Has_Written))));

            pragma Assert
              (To_String (Append (Pcd_Entry_String,
               Buf,
               Has_Written + Has_Written_B))
               = To_String (Pcd_Entry_String)
               & To_String
                 (Buf (Buf'First
                  ..
                    Natural'Last - Length (Old_String) - 1 + Buf'First
                  + Integer (Has_Written))));

            pragma Assert
              (To_String (String)
               = To_String (Append (Pcd_Entry_String,
                 Buf,
                 Has_Written + Has_Written_B)));
            --  The three assertions above are small steps to prove the
            --  following.

            pragma Assert
              (String
               = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
         end if;
      end Prove_Postcondition_String_Version;
   begin
      Prove_Postcondition_String_Version
        (Element (Contents, Fd),
         Element (Contents_Old, Fd),
         Element (Contents_Pcd_Entry, Fd));
      --  Call to the same procedure but with Unbounded_String arguments.

   end Prove_Full_Write_LI;

end Lemmas;
