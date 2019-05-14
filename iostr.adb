with Ada.Unchecked_Deallocation;
--with Ada.Text_IO; use Ada.Text_IO;
package body Iostr with
  SPARK_Mode => Off
is

   procedure Resize (Source : in out Unbounded_String; Length : Natural);

   function Append
     (Left  : Unbounded_String;
      Right : Init_String;
      Bytes : Int)
      return     Unbounded_String
   is
      New_Length : Natural := Left.Length + Natural (Bytes);
      Result : Unbounded_String := Unbounded_String'(Length => New_Length,
                                 Content => Init_Content (New_Length));
   begin
      if Left.Length > 0 then
         Result.Content (1 .. Left.Length) := Left.Content (1 .. Left.Length);
      end if;

      for J in 1 .. Natural (Bytes) loop
        Result.Content (Left.Length + J) := Right (Right'First - 1 + J);
      end loop;

      return Result;
   end Append;

   procedure Append_Pcd
     (Left  : in out Unbounded_String;
      Right : in Init_String;
      Bytes : ssize_t)
   is
      New_Length : Natural := Left.Length + Natural (Bytes);
      Content : String := Left.Content (1 .. Left.Length);
   begin
      Resize (Left, Natural (Bytes));

      Left.Content (1 .. Left.Length) := Content;
      for J in 1 .. Natural (Bytes) loop
        Left.Content (Left.Length + J) := Right (Right'First - 1 + J);
      end loop;

      Left.Length := New_Length;
   end Append_Pcd;

   procedure Equals (L, R : Unbounded_String) is begin null; end Equals;

   function Init_Content (L : Natural := 100) return String_Access is
   begin
      return new String (1 .. L);
   end Init_Content;

   function Length (Source : Unbounded_String) return Natural is (Source.Length);

   procedure Resize (Source : in out Unbounded_String; Length : Natural) is
   begin
      if Source.Length + Length <= Source.Content'Last then
         return;
      end if;
      declare
         procedure Finalize is new Ada.Unchecked_Deallocation
           (Object => String,
            Name   => String_Access_Base);

         New_Length : constant Natural :=
           (if Source.Length > (Natural'Last / 2) - Length then Natural'Last
            else (Source.Length + Length) * 2);
         Str        : String_Access := Init_Content (New_Length);
         Old_Str    : String_Access_Base := Source.Content;
      begin
         Str (1 .. Source.Length) := Source.Content (1 .. Source.Length);
         Source.Content := Str;
         Finalize (Old_Str);
      end;
   end Resize;

   function To_String (Source : Init_String) return String is
      Result : String (1 .. Source'Length);
   begin
      for J in 1 .. Source'Length loop
         Result (J) := Source (Source'First - 1 + J);
      end loop;
      return Result;
   end To_String;

   function To_String (Source : Unbounded_String) return String is
     Result : String (1 .. Source.Length);
   begin
      for J in 1 .. Source.Length loop
         Result (J) := Source.Content (J);
      end loop;
      return Result;
   end To_String;
end Iostr;
