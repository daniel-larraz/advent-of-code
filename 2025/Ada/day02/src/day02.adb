pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day02 is

   procedure Get (
      File : File_Type;
      N    : out Big_Integer)
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (End_Error | Invalid_Input => True)
   is
      C   : Character;
      EOL : Boolean;
   begin
      N := 0;
      Look_Ahead (File, C, EOL);
      while not EOL and then C in '0' .. '9' loop
         Get (File, C);
         Check (C in '0' .. '9', "Error: Unexpected character");
         N := 10 * N + To_Big_Integer (To_Digit (C));
         Look_Ahead (File, C, EOL);
      end loop;
   end Get;

   procedure Read_Range (
      File     : File_Type;
      First_Id : out Big_Integer;
      Last_Id  : out Big_Integer)
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (End_Error | Invalid_Input => True)
   is
      Sep : Character;
   begin
      Last_Id := 0;
      Get (File, First_Id);
      Get (File, Sep);
      Check (Sep = '-', "Error: Found invalid ID separator.");
      Get (File, Last_Id);
   end Read_Range;

   function Is_Invalid_Id_Part1 (Id : Big_Integer) return Boolean is
      Id_Str : constant String :=
         Ada.Strings.Fixed.Trim (To_String (Id), Ada.Strings.Both);
      Len    : constant Natural := Id_Str'Length;
      Half   : Natural;
   begin
      if Len mod 2 /= 0 then
         return False;
      end if;

      Half := Len / 2;

      return Id_Str (Id_Str'First .. Id_Str'First + Half - 1) =
         Id_Str (Id_Str'First + Half .. Id_Str'Last);
   end Is_Invalid_Id_Part1;

   procedure Lemma_Product_Leq_From_Quotient (
      Arg1 : Natural;
      Arg2 : Integer;
      Arg3 : Positive)
   with
      Ghost,
      Pre  => Arg2 mod Arg3 = 0 and then Arg1 <= Arg2 / Arg3,
      Post => Arg1 * Arg3 <= Arg2
   is
   begin
      null;
   end Lemma_Product_Leq_From_Quotient;

   function Is_Invalid_Id_Part2 (Id : Big_Integer) return Boolean is
      Id_Str : constant String :=
         Ada.Strings.Fixed.Trim (To_String (Id), Ada.Strings.Both);
      Len : constant Natural := Id_Str'Length;
   begin
      for K in 1 .. Len / 2 loop
         if Len mod K = 0 then
            declare
               Prefix  : constant String :=
                  Id_Str (Id_Str'First .. Id_Str'First + K - 1);
               Repeats : constant Integer := Len / K;
               Rebuild : Unbounded_String := To_Unbounded_String ("");
            begin
               for I in 1 .. Repeats loop
                  Lemma_Product_Leq_From_Quotient (I, Len, K);
                  pragma Loop_Invariant (K <= Len - (I - 1) * K);
                  pragma Loop_Invariant (Prefix'Length = K);
                  pragma Loop_Invariant (Length (Rebuild) = (I - 1) * K);
                  pragma Loop_Invariant
                     (Prefix'Length <= Natural'Last - Length (Rebuild));
                  Append (Rebuild, Prefix);
               end loop;

               if To_String (Rebuild) = Id_Str and then Repeats >= 2
               then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end Is_Invalid_Id_Part2;

   function Is_Invalid_Id (
      Part : Puzzle_Part;
      Id : Big_Integer
   ) return Boolean is
   begin
      if Part = Part_1 then
         return Is_Invalid_Id_Part1 (Id);
      else
         return Is_Invalid_Id_Part2 (Id);
      end if;
   end Is_Invalid_Id;

   procedure Update_Answer (
      Part     : Puzzle_Part;
      First_Id : Big_Integer;
      Last_Id  : Big_Integer;
      Answer   : in out Big_Integer)
   is
      Id : Big_Integer;
   begin
      Id := First_Id;
      while Id <= Last_Id loop
         if Is_Invalid_Id (Part, Id) then
            Answer := Answer + Id;
         end if;
         Id := Id + 1;
      end loop;
   end Update_Answer;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      File     : File_Type;
      First_Id : Big_Integer;
      Last_Id  : Big_Integer;
      Sep      : Character;
      Answer   : Big_Integer := 0;
   begin
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         Check (not End_Of_File (File), "Error: Input file is empty.");
         Read_Range (File, First_Id, Last_Id);
         Update_Answer (Part, First_Id, Last_Id, Answer);
         while not End_Of_File (File) loop
            Get (File, Sep);
            Check (Sep = ',', "Error: Found invalid range separator.");
            Read_Range (File, First_Id, Last_Id);
            Update_Answer (Part, First_Id, Last_Id, Answer);
         end loop;
         Put ("Answer:"); Put_Line (To_String (Answer));
         Close (File);
      end if;
      pragma Assert (not Is_Open (File));
   exception
      when End_Error =>
         Put_Line ("Error: Reached end of input unexpectedly.");
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day02/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day02/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day02/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day02/input.txt");
end Day02;