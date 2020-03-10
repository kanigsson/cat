with Interfaces.C; use Interfaces.C;
private with Ada.Strings.Unbounded;

package Iostr with
  SPARK_Mode => On
is
   pragma Annotate (GNATProve, Terminating, Iostr);

   subtype ssize_t is int;

   type Init_String is new String with Relaxed_Initialization;

   subtype One_String is String
   with Predicate => One_String'First = 1;

   function My_Eq (A, B : One_String) return Boolean is
      (A'Last = B'Last and then
         (for all I in A'Range => A (I) = B (I)));

   function Append (A, B : One_String) return One_String is
     (if Integer'Last - B'Length < A'Length then
         A & B (B'First .. Integer'Last - A'Length)
      else A & B);

   function Append (A, B : One_String; Bytes : int) return One_String is
     (if Integer'Last - Integer (Bytes) < A'Length then
           A & B (B'First .. Integer'Last - A'Length)
      else A & B (B'First .. Integer (Bytes)))
   with Pre => 0 <= Bytes and then Bytes <= B'Length;

   function Is_Append (A, B, C : One_String) return Boolean is
     (C'Length = (if Integer'Last - B'Length < A'Length then Integer'Last
                  else A'Length + B'Length)
      and then
        (for all I in C'Range =>
              C (I) = (if I <= A'Length then A (I) else B (I - A'Length))));

   function Is_Append (A : One_String; B : Init_String; C : One_String;
                       Bytes : int) return Boolean is
     (C'Length = (if Integer'Last - Integer (Bytes) < A'Length then
                     Integer'Last
                  else A'Length + Natural (Bytes))
      and then
        (for all I in C'Range =>
              C (I) = (if I <= A'Length then A (I)
                       else B (B'First - 1 + (I - A'Length)))))
       with Pre => 0 <= Bytes and then Bytes <= B'Length
                   and then B (B'First ..
                               B'First - 1 + Natural (Bytes))'Initialized;

end Iostr;
