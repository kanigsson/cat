with Iostr; use Iostr;
with Interfaces.C; use Interfaces.C;

with Contents_Table_Type; use Contents_Table_Type;
use Contents_Table_Type.Maps;

package Lemmas with SPARK_Mode is

   --  appending M chars, then N of the same buffer is the same as appending
   --  (M+N) chars
   procedure Lemma_Is_Append_Trans
     (A, B, C : One_String; I : Init_String;
      M, N : int)
     with Ghost,
     Pre =>
         0 <= M and then Integer (M) <= I'Length
       and then
         0 <= N and then Integer (N) <= I'Length
       and then
         int'Last - N >= M
       and then
         0 <= M + N and then Integer (M + N) <= I'Length
       and then
         Integer (int'Last - M) >= I'First
       and then
         I (I'First ..  I'First - 1 + Natural (M + N))'Initialized
       and then
         Is_Append (A, I, B, M)
       and then
         Is_Append (B, I (I'First + Natural (M) .. I'Last), C, N),
     Post =>
         Is_Append (A, I, C, M + N);

   --  If C is obtained by Append of A and B, and B is extended to B2, and C is
   --  extended in the same way to C2, then C2 is obtained by Append of A and
   --  B2
   procedure Lemma_Is_Append_Trans
     (A, B, B2, C, C2 : One_String; I : Init_String; M : int)
   with Ghost,
     Pre =>
         0 <= M and then Integer (M) <= I'Length
       and then
         Is_Append (A, B, C)
       and then
         I (I'First ..  I'First - 1 + Natural (M))'Initialized
       and then
         Is_Append (B, I, B2, M)
       and then
         Is_Append (C, I, C2, M),
     Post =>
       Is_Append (A, B2, C2);

   --  Some property (here: Is_Append) concerning the content of B at Fd1
   --  is also true for C at Fd1, if B only differs from C at the different
   --  location Fd2.
   procedure Lemma_Is_Append_Equal_Except_Inv
     (A, B, C : Map; Fd1, Fd2 : int; I : Init_String; M : int)
   with Ghost,
   Pre =>
       Fd1 /= Fd2 and then
       Has_Key (A, Fd1) and then Has_Key (B, Fd1) and then Has_Key (C, Fd1)
     and then
     0 <= M and then Integer (M) <= I'Length and then
     I (I'First ..  I'First - 1 + Natural (M))'Initialized and then
     Is_Append (Get (A, Fd1), I, Get (B, Fd1), M) and then
     Elements_Equal_Except (B, C, Fd2),
   Post =>
     Is_Append (Get (A, Fd1), I, Get (C, Fd1), M);

   --  same as above, but for the left side of Is_Append
   procedure Lemma_Is_Append_Equal_Except_Inv2
     (A, B, C : Map; Fd1, Fd2 : int; I : Init_String; M : int)
   with Ghost,
   Pre =>
       Fd1 /= Fd2 and then
       Has_Key (A, Fd2) and then Has_Key (B, Fd2) and then Has_Key (C, Fd2)
     and then
     0 <= M and then Integer (M) <= I'Length and then
     I (I'First ..  I'First - 1 + Natural (M))'Initialized and then
     Is_Append (Get (B, Fd2), I, Get (C, Fd2), M) and then
     Elements_Equal_Except (A, B, Fd1),
   Post =>
     Is_Append (Get (A, Fd2), I, Get (C, Fd2), M);


   --  If A differs from B in only one element X, and B from C in only one
   --  element Y, then A differs from C only in X and Y.
   procedure Lemma_Equal_Except_Trans
      (A, B, C : Map; X, Y : int)
      with Ghost,
      Pre =>
         Elements_Equal_Except (A, B, X)
         and then
         Elements_Equal_Except (B, C, Y),
     Post => Elements_Equal_Except (A, C, X, Y);

   procedure Lemma_Equal_Except2_Trans
     (A, B, C : Map; X, Y : int)
     with Ghost,
      Pre =>
           Elements_Equal_Except (A, B, X, Y)
         and then
           Elements_Equal_Except (B, C, X, Y),
     Post =>
       Elements_Equal_Except (A, C, X, Y);

   procedure Lemma_Equal_Except2_Id
     (A, B, C : Map; X, Y : int)
     with Ghost,
      Pre =>
       Elements_Equal_Except (A, B, X, Y) and then
       B = C,
     Post =>
       Elements_Equal_Except (A, C, X, Y);

   procedure Lemma_Equal_Except21_Trans
     (A, B, C : Map; X, Y : int)
     with Ghost,
      Pre =>
           Elements_Equal_Except (A, B, X, Y)
         and then
           Elements_Equal_Except (B, C, Y),
     Post =>
       Elements_Equal_Except (A, C, X, Y);
end Lemmas;
