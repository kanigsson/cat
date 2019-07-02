package body Lemmas with
  SPARK_Mode => On
is
   procedure Aux (A, B, C : String) with
     Pre =>
       A = B
     and then
       A'First = 1
     and then
       B'First = 1
     and then
       C'Length <= Natural'Last - A'Length,
     Post => A & C = B & C;
   procedure Aux (A, B, C : String) is null;

   procedure Aux_1 (Str, L_1, L_2, R : String) with
        Pre =>
          L_1 = L_2
            and then L_1'Length <= Natural'Last - R'Length
            and then L_1'First = 1
            and then L_2'First = 1
            and then R'First = 1
            and then Str = L_1 & R,
        Post => Str = L_2 & R;
   procedure Aux_1 (Str, L_1, L_2, R : String) is null;

   procedure Aux_2 (Str, L, R_1, R_2 : String) with
     Pre =>
       R_1 = R_2
         and then R_1'Length <= Natural'Last - L'Length
         and then R_1'First = 1
         and then R_2'First = 1
         and then L'First = 1
         and then Str = L & R_1,
      Post => Str = L & R_2;
   procedure Aux_2 (Str, L, R_1, R_2 : String) is null;

   procedure Aux_3 (A, B, C, D : String) with
     Pre =>
       A'First = 1
         and then B'First = 1
         and then C'First = 1
         and then D'First = 1
         and then D'Length <= Natural'Last - B'Length - C'Length
         and then A = B & C & D,
    Post => A = B & (C & D);
   procedure Aux_3 (A, B, C, D : String) is null;

   procedure Aux_4 (A, B, C, D : String) with
     Pre =>
       A'First = 1
         and then B'First = 1
         and then C'First = 1
         and then D'First = 1
         and then D'Length <= Natural'Last - C'Length
         and then A = B
         and then A = C & D,
     Post => B = C & D;
   procedure Aux_4 (A, B, C, D : String) is null;

   procedure Aux_5 (A, B, C, D, E : String) with
     Pre =>
       A'First = 1
         and then B'First = 1
         and then D'First = 1
         and then E'First = 1
         and then D'Length <= Natural'Last - E'Length
         and then C'Length <= Natural'Last - B'Length
         and then A = B & C
         and then B = D & E,
       Post => A = D & E & C;
   procedure Aux_5 (A, B, C, D, E : String) is null;

   procedure Init_String_Equal_To_String (Buf, L, R : Init_String) with
     Pre =>
       Buf'Valid_Scalars
         and then L'Valid_Scalars
         and then R'Valid_Scalars
         and then L'Length <= Natural'Last - R'Length
         and then L'Last <= Natural'Last - R'Length
         and then Buf = L & R,
      Post => To_String (Buf) = To_String (L) & To_String (R);
   procedure Init_String_Equal_To_String (Buf, L, R : Init_String) is
   begin
      null;
   end Init_String_Equal_To_String;

   procedure Append_Length
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
        elsif Natural (Num_Bytes) <= Natural'Last - Length (A)
          then Length (Append (A, Buf, Num_Bytes))
               = Length (A) + Natural (Num_Bytes)
        else Length (Append (A, Buf, Num_Bytes)) = Natural'Last);
   procedure Append_Length
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
   end Append_Length;

   procedure Reverse_Append_Length_Lemma
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
         else Natural (Num_Bytes) < Natural'Last - Length (A));
   procedure Reverse_Append_Length_Lemma
     (A : Unbounded_String;
      Buf : Init_String;
      Num_Bytes : ssize_t) is null;

   procedure Equal_And_Append
     (Str, Left_1, Left_2 : Unbounded_String;
      Buf                 : Init_String;
      Bytes               : int)
   is

   begin
      pragma Assert (To_String (Left_1) = To_String (Left_2));

      if Natural (Bytes) <= Natural'Last - Length (Left_1) then
         Aux (To_String (Left_1),
              To_String (Left_2),
              To_String (Buf (Buf'First .. Buf'First - 1 + Natural (Bytes))));
         pragma Assert
           (To_String (Left_1)
            & To_String (Buf (Buf'First .. Buf'First - 1 + Natural (Bytes)))
            = To_String (Left_2)
            & To_String (Buf (Buf'First .. Buf'First - 1 + Natural (Bytes))));
         pragma Assert
           (Append (Left_1, Buf, Bytes) = Append (Left_2, Buf, Bytes));
      else
         declare
            New_Bytes : constant Natural := Natural'Last - Length (Left_1);
         begin
            Aux (To_String (Left_1),
                 To_String (Left_2),
                 To_String (Buf (Buf'First
                                  ..
                                 Buf'First - 1 + Natural (New_Bytes))));
            pragma Assert
              (To_String (Left_1)
               & To_String (Buf (Buf'First .. Buf'First - 1 + New_Bytes))
               = To_String (Left_2)
               & To_String (Buf (Buf'First .. Buf'First - 1 + New_Bytes)));
            pragma Assert
              (Append (Left_1, Buf, Bytes) = Append (Left_2, Buf, Bytes));
         end;
      end if;
   end Equal_And_Append;

   procedure Prove_Equality
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Read                                   : ssize_t;
      Input, Stdout                              : int)
   is
   begin
      Substit (Element (Contents, Stdout).String,
               Element (Contents_Old, Stdout).String,
               Element (Contents_Pcd_Entry, Stdout).String,
               Element (Contents_Old, Input).String,
               Buf,
               Has_Read);
      Substit_2 (Element (Contents, Stdout).String,
                 Element (Contents_Pcd_Entry, Stdout).String,
                 Element (Contents_Old, Input).String,
                 Buf,
                 Has_Read);
      Substit_3 (Element (Contents, Stdout).String,
                 Element (Contents_Pcd_Entry, Stdout).String,
                 Element (Contents_Old, Input).String,
                 Element (Contents, Input).String,
                 Buf,
                 Has_Read);
   end Prove_Equality;

   procedure Prove_Loop_Invariant
     (Contents, Contents_Old, Contents_Pcd_Entry : Map;
      Buf                                        : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S    : ssize_t;
      Fd                                         : int)
   is
   begin
      Prove_Loop_Invariant_String_Version
        (Element (Contents, Fd).String,
         Element (Contents_Old, Fd).String,
         Element (Contents_Pcd_Entry, Fd).String,
         Buf,
         Has_Written,
         Has_Written_B,
         Num_Bytes_S);
   end Prove_Loop_Invariant;

   procedure From_Str_To_Unb_Str
     (String, Old_String, Pcd_Entry_String    : Unbounded_String;
      Buf                                     : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S : ssize_t)
   with
     Pre =>
       Num_Bytes_S in 1 .. ssize_t (Integer'Last)
         and then Integer (Num_Bytes_S) <= Buf'Length
         and then Buf'Last < Natural'Last
         and then Has_Written in 0 .. Num_Bytes_S
         and then Has_Written_B in 0 .. Num_Bytes_S - Has_Written
         and then
       Buf (Buf'First .. Buf'First - 1 + Natural (Num_Bytes_S))'Valid_Scalars
         and then
         (if Integer (Has_Written + Has_Written_B)
             <= Natural'Last - Length (Pcd_Entry_String)
         then
       To_String (String)
            = To_String (Pcd_Entry_String)
            & To_String
                (Buf (Buf'First
                       ..
                      Buf'First - 1 + Integer (Has_Written + Has_Written_B)))
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
     Post =>
       String = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B);
   procedure From_Str_To_Unb_Str
     (String, Old_String, Pcd_Entry_String    : Unbounded_String;
      Buf                                     : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S : ssize_t) is
   begin
      if Integer (Has_Written + Has_Written_B)
         > Natural'Last - Length (Pcd_Entry_String)
      then
         pragma Assert
           (Length (Old_String) - Integer (Has_Written)
            = Length (Pcd_Entry_String));
         pragma Assert
           (To_String (String)
            = To_String (Pcd_Entry_String)
            & To_String
                (Buf (Buf'First
                        ..
                      Natural'Last - Length (Pcd_Entry_String) - 1
                      + Buf'First)));
         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
      else
         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
      end if;
   end From_Str_To_Unb_Str;
   procedure Prove_Loop_Invariant_String_Version
     (String, Old_String, Pcd_Entry_String    : Unbounded_String;
      Buf                                     : Init_String;
      Has_Written, Has_Written_B, Num_Bytes_S : ssize_t)
   is
   begin
      pragma Assert
        (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))
         & Buf (Buf'First + Integer (Has_Written) .. Buf'Last)
             (Buf'First + Integer (Has_Written)
                ..
              Buf'First - 1 + Integer (Has_Written) + Integer (Has_Written_B))
         = Buf (Buf'First
           ..
             Buf'First - 1 + Integer (Has_Written) + Integer (Has_Written_B)));

      if Has_Written_B = 0 then
         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));

      elsif Length (Old_String) = Natural'Last then
         pragma Assert
           (To_String (String) = To_String (Old_String));
         pragma Assert
           (To_String (Old_String)
            = To_String (Append (Old_String,
              Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
              Has_Written_B)));
         pragma Assert (To_String (String) = To_String (Old_String));
         pragma Assert
           (To_String (Old_String)
            = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));

         if Length (Pcd_Entry_String) = Natural'Last then
            pragma Assert
              (To_String (Append (Pcd_Entry_String,
                                  Buf,
                                  Has_Written + Has_Written_B))
               = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));
         else
            Reverse_Append_Length_Lemma (Pcd_Entry_String, Buf, Has_Written);
            pragma Assert
              (Natural (Has_Written)
               >= Natural'Last - Length (Pcd_Entry_String));

            if Natural (Has_Written) = Natural'Last - Length (Pcd_Entry_String)
            then
               pragma Assert
                 (To_String (Append (Pcd_Entry_String,
                                     Buf,
                                     Has_Written + Has_Written_B))
                  = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));
            else
               pragma Assert
                 (To_String (Append (Pcd_Entry_String,
                                     Buf,
                                     Has_Written + Has_Written_B))
                  = To_String (Append (Pcd_Entry_String, Buf, Has_Written)));
            end if;
         end if;

         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));

      elsif Integer (Has_Written_B) <= Natural'Last - Length (Old_String) then

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
         Aux_4 (To_String (Append (Old_String,
                Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                Has_Written_B)),
                To_String (String),
                To_String (Old_String),
                To_String
                  (Buf (Buf'First + Integer (Has_Written)
                         ..
                        Buf'First + Integer (Has_Written) - 1
                        + Natural (Has_Written_B))));
         pragma Assert (Length (Old_String) < Natural'Last);
         pragma Assert
           (Length (Old_String)
            = Length (Append (Pcd_Entry_String, Buf, Has_Written)));
         Reverse_Append_Length_Lemma (Pcd_Entry_String, Buf, Has_Written);
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
         Aux_4 (To_String (Append (Pcd_Entry_String, Buf, Has_Written)),
                To_String (Old_String),
                To_String (Pcd_Entry_String),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Written))));

         Aux_5 (To_String (String),
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

         Aux_3 (To_String (String),
                To_String (Pcd_Entry_String),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Written))),
                To_String
                  (Buf (Buf'First + Integer (Has_Written)
                     ..
                    Buf'First + Integer (Has_Written) - 1
                    + Natural (Has_Written_B))));

         pragma Assert
           (Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written))
            & Buf (Buf'First + Integer (Has_Written) .. Buf'Last)
            (Buf'First + Integer (Has_Written)
              ..
             Buf'First - 1 + Integer (Has_Written) + Integer (Has_Written_B))
            = Buf (Buf'First
                    ..
                   Buf'First - 1 + Integer (Has_Written)
                   + Integer (Has_Written_B)));
         Init_String_Equal_To_String
           (Buf (Buf'First
            ..
              Buf'First - 1 + Integer (Has_Written) + Integer (Has_Written_B)),
            Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written)),
            Buf (Buf'First + Integer (Has_Written)
                   ..
                 Buf'First + Integer (Has_Written) - 1
                 + Natural (Has_Written_B)));
         Aux_2 (To_String (String),
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
         pragma Assert
           (Integer (Has_Written) + Integer (Has_Written_B)
            <= Natural'Last - Length (Pcd_Entry_String));
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

         From_Str_To_Unb_Str (String,
                              Old_String,
                              Pcd_Entry_String,
                              Buf,
                              Has_Written,
                              Has_Written_B,
                              Num_Bytes_S);
         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));

      else

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
         Aux_4 (To_String (Append (Old_String,
                Buf (Buf'First + Integer (Has_Written) .. Buf'Last),
                Has_Written_B)),
                To_String (String),
                To_String (Old_String),
                To_String
                  (Buf (Buf'First + Integer (Has_Written)
                         ..
                        Natural'Last - Length (Old_String) - 1 + Buf'First
                        + Integer (Has_Written))));
         pragma Assert (Length (Old_String) < Natural'Last);
         pragma Assert
           (Length (Old_String)
            = Length (Append (Pcd_Entry_String, Buf, Has_Written)));
         Reverse_Append_Length_Lemma (Pcd_Entry_String, Buf, Has_Written);
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
         Aux_4 (To_String (Append (Pcd_Entry_String, Buf, Has_Written)),
                To_String (Old_String),
                To_String (Pcd_Entry_String),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Written))));

         Aux_5 (To_String (String),
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

         Aux_3 (To_String (String),
                To_String (Pcd_Entry_String),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Written))),
                To_String
                  (Buf (Buf'First + Integer (Has_Written)
                         ..
                        Natural'Last - Length (Old_String) - 1 + Buf'First
                        + Integer (Has_Written))));

         Init_String_Equal_To_String
           (Buf (Buf'First
                   ..
                 Natural'Last - Length (Old_String) - 1 + Buf'First
                 + Integer (Has_Written)),
            Buf (Buf'First .. Buf'First - 1 + Natural (Has_Written)),
            Buf (Buf'First + Integer (Has_Written)
                  ..
                 Natural'Last - Length (Old_String) - 1 + Buf'First
                 + Integer (Has_Written)));
         Aux_2 (To_String (String),
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
         pragma Assert
           (To_String (String)
            = To_String (Pcd_Entry_String)
            & To_String
                (Buf (Buf'First
                       ..
                      Natural'Last - Length (Old_String) - 1 + Buf'First
                      + Integer (Has_Written))));

         pragma Assert
           (To_String (String)
            = To_String (Pcd_Entry_String)
            & To_String
                (Buf (Buf'First
                       ..
                      Natural'Last - Length (Old_String) - 1 + Buf'First
                      + Integer (Has_Written))));

         From_Str_To_Unb_Str (String,
                              Old_String,
                              Pcd_Entry_String,
                              Buf,
                              Has_Written,
                              Has_Written_B,
                              Num_Bytes_S);
         pragma Assert
           (String
            = Append (Pcd_Entry_String, Buf, Has_Written + Has_Written_B));
      end if;
   end Prove_Loop_Invariant_String_Version;

   procedure Substit
     (A, B, C, D : Unbounded_String;
      Buf        : Init_String;
      Has_Read   : ssize_t)
   is
   begin

      if Length (B) = Natural'Last or else Has_Read = 0 then
         pragma Assert (A = Append (C & D, Buf, Has_Read));

      elsif Natural (Has_Read) <= Natural'Last - Length (B) then

         Aux_1 (To_String (A),
                To_String (B),
                To_String (C & D),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Read))));
         pragma Assert (A = Append (C & D, Buf, Has_Read));

      else
         pragma Assert
           (To_String (A)
            = To_String (B)
            & To_String (Buf (Buf'First
                               ..
                              Natural'Last - Length (B) - 1 + Buf'First)));
         Aux_1 (To_String (A),
                To_String (B),
                To_String (C & D),
                To_String (Buf (Buf'First
                                 ..
                                Natural'Last - Length (B) - 1 + Buf'First)));
         pragma Assert (Length (B) = Length (C & D));
         pragma Assert (Natural (Has_Read) > Natural'Last - Length (C & D));
         pragma Assert (A = Append (C & D, Buf, Has_Read));
      end if;
   end Substit;

   procedure Substit_2
     (A, B, C  : Unbounded_String;
      Buf      : Init_String;
      Has_Read : ssize_t)
   is

      procedure Null_Has_Read with
        Pre =>
          Has_Read = 0
            and then
          A = Append (B & C, Buf, Has_Read),
        Post => A = B & Append (C, Buf, Has_Read);

      procedure Null_Has_Read is
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
         Equal_String (A, Append (B & C, Buf, Has_Read), B, C);
         Lemma (C, Buf, Has_Read);
         Equal_And_Append (A, B, C, Append (C, Buf, Has_Read));
      end Null_Has_Read;

      procedure Append_B_C_Has_Max_Length with
        Pre =>
          Has_Read in 1 .. Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
            and then
          A = Append (B & C, Buf, Has_Read)
            and then
          Length (B & C) = Natural'Last,
        Post => A = B & Append (C, Buf, Has_Read);
      procedure Append_B_C_Has_Max_Length is
      begin
         pragma Assert (Length (C) >= Natural'Last - Length (B));
         Append_Length (C, Buf, Has_Read);
         pragma Assert
           (To_String (Append (B & C, Buf, Has_Read))
           = To_String (B & C));
         pragma Assert (To_String (A) = To_String (B & C));
         pragma Assert (To_String (A) = To_String (B)
                        & To_String (C) (1 .. Natural'Last - Length (B)));
         if Has_Read = 0 or else Length (C) = Natural'Last then
            pragma Assert (To_String (Append (C, Buf, Has_Read))
                             (1 .. Natural'Last - Length (B))
                           = To_String (C) (1 .. Natural'Last - Length (B)));
         elsif Natural (Has_Read) <= Natural'Last - Length (C) then
            pragma Assert (To_String (Append (C, Buf, Has_Read))
                             (1 .. Natural'Last - Length (B))
                           = To_String (C) (1 .. Natural'Last - Length (B)));
         else
            pragma Assert (To_String (Append (C, Buf, Has_Read))
                             (1 .. Natural'Last - Length (B))
                           = To_String (C) (1 .. Natural'Last - Length (B)));
         end if;

         pragma Assert (A = B & Append (C, Buf, Has_Read));
      end Append_B_C_Has_Max_Length;

      procedure Has_Read_Less_Than_Length with
        Pre =>
          Has_Read in 1 .. Buf'Length
            and then
          Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
            and then
          A = Append (B & C, Buf, Has_Read)
            and then
          Natural (Has_Read) <= Natural'Last - Length (B & C)
            and then
          Natural'Last /= Length (B & C),
        Post => A = B & Append (C, Buf, Has_Read);
      procedure Has_Read_Less_Than_Length is
      begin
         Aux_1 (To_String (A),
                To_String (B & C),
                To_String (B) & To_String (C),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Read))));
         Aux_3 (To_String (A),
                To_String (B),
                To_String (C),
                To_String (Buf (Buf'First
                                 ..
                                Buf'First - 1 + Natural (Has_Read))));
         Aux_2 (To_String (A),
                To_String (B),
                To_String (C)
                & To_String
                  (Buf (Buf'First
                   ..
                     Buf'First - 1 + Natural (Has_Read))),
                To_String (Append (C, Buf, Has_Read)));
         pragma Assert
           (To_String (A)
            = To_String (B)
            & To_String (Append (C, Buf, Has_Read)));
         pragma Assert (A = B & Append (C, Buf, Has_Read));
      end Has_Read_Less_Than_Length;

      procedure Has_Read_Greater_Than_Length with
         Pre =>
           Has_Read in 1 .. Buf'Length
             and then
           Buf (Buf'First .. Buf'First - 1 + Natural (Has_Read))'Valid_Scalars
             and then
           A = Append (B & C, Buf, Has_Read)
             and then
           Natural'Last /= Length (B & C)
             and then
           Natural (Has_Read) > Natural'Last - Length (B & C),
         Post => A = B & Append (C, Buf, Has_Read);
      procedure Has_Read_Greater_Than_Length is
      begin
         Aux_1 (To_String (A),
                To_String (B & C),
                To_String (B) & To_String (C),
                To_String (Buf (Buf'First
                  ..
                    Natural'Last - Length (B) - Length (C) - 1 + Buf'First)));
         Aux_3 (To_String (A),
                To_String (B),
                To_String (C),
                To_String (Buf (Buf'First
                                 ..
                                Natural'Last - Length (B) - Length (C) - 1
                                + Buf'First)));
         pragma Assert (Length (B & C) /= Natural'Last
                        and then Length (B & C) = Length (B) + Length (C));
         pragma Assert
           (Natural (Has_Read)
            > Natural'Last - Length (B) - Length (C));

         if Natural (Has_Read) <= Natural'Last - Length (C) then
            pragma Assert
              (Length (Append (C, Buf, Has_Read))
               >= Natural'Last - Length (B));
         else
            pragma Assert
              (Length (Append (C, Buf, Has_Read))
               >= Natural'Last - Length (B));
         end if;

         pragma Assert
           (To_String (C)
            & To_String (Buf (Buf'First
                               ..
                              Natural'Last - Length (B) - Length (C) - 1
                              + Buf'First))
            = To_String (Append (C, Buf, Has_Read))
                (1 .. Natural'Last - Length (B)));
         pragma Assert
           (Length (Append (C, Buf, Has_Read))
            >= Natural'Last - Length (B));
         Aux_2 (To_String (A),
                To_String (B),
                To_String (C)
                & To_String (Buf (Buf'First
                                   ..
                                  Natural'Last - Length (B) - Length (C) - 1
                                  + Buf'First)),
                To_String (Append (C, Buf, Has_Read))
                  (1 .. Natural'Last - Length (B)));
         pragma Assert
           (To_String (A)
            = To_String (B)
            & To_String (Append (C, Buf, Has_Read))
                (1 .. Natural'Last - Length (B)));
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
   end Substit_2;

end Lemmas;
