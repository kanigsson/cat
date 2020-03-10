package body Lemmas with SPARK_Mode is

   procedure Lemma_Is_Append_Trans
     (A, B, C : One_String; I : Init_String;
      M, N : int) is
   null;

   procedure Lemma_Is_Append_Trans
     (A, B, B2, C, C2 : One_String; I : Init_String; M : int) is null;

   procedure Lemma_Equal_Except_Trans
     (A, B, C : Map; X, Y : int) is null;

   procedure Lemma_Is_Append_Equal_Except_Inv
     (A, B, C: Map; Fd1, Fd2 : Int; I : Init_String; M : Int) is null;

   procedure Lemma_Is_Append_Equal_Except_Inv2
     (A, B, C: Map; Fd1, Fd2 : Int; I : Init_String; M : Int) is null;

   procedure Lemma_Equal_Except2_Trans
     (A, B, C : Map; X, Y : int) is null;

   procedure Lemma_Equal_Except2_Id
     (A, B, C : Map; X, Y : int) is null;

   procedure Lemma_Equal_Except21_Trans
     (A, B, C : Map; X, Y : int) is null;
end Lemmas;
