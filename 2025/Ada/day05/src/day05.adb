pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

procedure Day05 is

   type Id_Range_Type is record
      First : Big_Integer;
      Last  : Big_Integer;
   end record;

   function "<" (R1 : Id_Range_Type; R2 : Id_Range_Type) return Boolean is
      (R1.First < R2.First);

   package Fresh_DB_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Id_Range_Type);

   package Fresh_DB_Sorting is new Fresh_DB_Pkg.Generic_Sorting;

   use Fresh_DB_Pkg;
   use Fresh_DB_Sorting;

   subtype Fresh_DB_Type is Fresh_DB_Pkg.Vector;

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
         Check (C in '0' .. '9', "Unexpected character");
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
      Check (Sep = '-', "Found invalid ID separator.");
      Get (File, Last_Id);
   end Read_Range;

   function Is_Fresh (
      Fresh_DB  : Fresh_DB_Type;
      Id        : Big_Integer)
   return Boolean is
   begin
      for C in Fresh_DB loop
         declare
            R : constant Id_Range_Type := Element (Fresh_DB, C);
         begin
            if R.First <= Id and then Id <= R.Last then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Fresh;

   procedure Count_Total_Fresh (
      Fresh_DB  : Fresh_DB_Type;
      Num_Fresh : out Big_Integer)
   is
      DB   : Fresh_DB_Type := Fresh_DB;
      Curr : Id_Range_Type;
   begin
      Num_Fresh := 0;
      if Length (DB) = 0 then
         return;
      end if;

      Sort (DB);
      Curr := Element (DB, 1);

      for I in First_Index (DB) + 1 .. Last_Index (DB) loop
         declare
            R : constant Id_Range_Type := Element (DB, I);
         begin
            if R.First <= Curr.Last + 1 then
               if R.Last > Curr.Last then
                  Curr.Last := R.Last;
               end if;
            else
               Num_Fresh := Num_Fresh + (Curr.Last - Curr.First + 1);
               Curr.First := R.First;
               Curr.Last  := R.Last;
            end if;
         end;
      end loop;
      Num_Fresh := Num_Fresh + (Curr.Last - Curr.First + 1);
   end Count_Total_Fresh;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      File      : File_Type;
      Fresh_DB  : Fresh_DB_Type;
      Id_Range  : Id_Range_Type;
      Id        : Big_Integer;
      UNUSED    : Character;
      EOL       : Boolean := False;
      Num_Fresh : Big_Integer;
   begin
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         while not EOL loop
            Read_Range (File, Id_Range.First, Id_Range.Last);
            Check (Length (Fresh_DB) < Last_Count,
               "Input length validation failed.");
            Append (Fresh_DB, Id_Range);
            Skip_Line (File);
            Look_Ahead (File, UNUSED, EOL);
         end loop;
         if Part = Part_1 then
            Num_Fresh := 0;
            while not End_Of_File (File) loop
               Skip_Line (File);
               Get (File, Id);
               if Is_Fresh (Fresh_DB, Id) then
                  Num_Fresh := Num_Fresh + 1;
               end if;
            end loop;
         else
            Count_Total_Fresh (Fresh_DB, Num_Fresh);
         end if;
         Put ("Number of fresh IDs:"); Put_Line (To_String (Num_Fresh));
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
   Process_File (Part_1, "./share/day05/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day05/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day05/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day05/input.txt");
end Day05;