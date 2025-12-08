pragma Spark_Mode (On);

with AOC; use AOC;
with AOC.Generic_Input; use AOC.Generic_Input;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

procedure Day06 is

   package Operands_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Natural);

   use Operands_Pkg;

   subtype Operands_Type is Operands_Pkg.Vector;

   procedure Read_Worksheet is new Read_Generic_Input (
      Max_Line_Length => 3750
   );

   procedure Get_Last_Col_of_Problem (
      Grid  : Grid_Type;
      First : Positive;
      Last  : out Positive
   )
   with
      Pre => not Is_Empty (Grid) and then Grid'First (2) <= First,
      Post => Last in Grid'Range (2) and then
              (Last = Grid'Last (2) or else Last <= Grid'Last (2) - 2),
      Exceptional_Cases => (Invalid_Input => True)
   is
   begin
      Check (First < Grid'Last (2), "Problem only has one column.");
      Last := First + 1;
      while Last < Grid'Last (2) and then Grid (Grid'Last (1), Last) = ' ' loop
         pragma Loop_Invariant (Grid'First (2) < Last);
         Last := Last + 1;
      end loop;
      if Last < Grid'Last (2) then
         Check (Last - First >= 2, "No space between problems.");
         Last := Last - 2;
      end if;
   end Get_Last_Col_of_Problem;

   procedure Get_Operands_Part1 (
      Grid     : Grid_Type;
      First    : Positive;
      Last     : Positive;
      Operands : out Operands_Type
   )
   with
      Pre => not Is_Empty (Grid) and then
         First in Grid'Range (2) and then Last in Grid'Range (2),
      Exceptional_Cases => (Invalid_Input => True)
   is
      O : String (First .. Last);
      N : Integer;
      UNUSED : Positive;
   begin
      Operands := Operands_Pkg.Empty_Vector;
      for I in Grid'First (1) .. Grid'Last (1) - 1 loop
         for J in First .. Last loop
            O (J) := Grid (I, J);
         end loop;
         Get (O, N, UNUSED);
         Check (N >= 0, "Parsed a negative number.");
         Check (Length (Operands) < Last_Count, "Too many operands.");
         Append (Operands, N);
      end loop;
   exception
      when Data_Error =>
         Put_Line ("Error while parsing a number.");
         raise Invalid_Input;
   end Get_Operands_Part1;

   procedure Get_Operands_Part2 (
      Grid     : Grid_Type;
      First    : Positive;
      Last     : Positive;
      Operands : out Operands_Type
   )
   with
      Pre => not Is_Empty (Grid) and then Grid'Length (1) >= 2 and then
         First in Grid'Range (2) and then Last in Grid'Range (2),
      Exceptional_Cases => (Invalid_Input => True)
   is
      O : String (Grid'First (1) .. Grid'Last (1) - 1);
      N : Integer;
      UNUSED : Positive;
   begin
      Operands := Operands_Pkg.Empty_Vector;
      for I in First .. Last loop
         for J in O'Range loop
            O (J) := Grid (J, I);
         end loop;
         Get (O, N, UNUSED);
         Check (N >= 0, "Parsed a negative number.");
         Check (Length (Operands) < Last_Count,
            "Operands length validation failed.");
         Append (Operands, N);
      end loop;
   exception
      when Data_Error =>
         Put_Line ("Error while parsing a number.");
         raise Invalid_Input;
   end Get_Operands_Part2;

   procedure Process_Worksheet (Part : Puzzle_Part; Grid : Grid_Type)
   with
      Pre => not Is_Empty (Grid) and then Grid'Length (1) >= 2
   is
      Total_Grand : Big_Integer := 0;
      First_Col   : Positive := Grid'First (2);
      Last_Col    : Positive;
      Operands    : Operands_Type;
      Result      : Big_Integer;
   begin
      loop
         pragma Loop_Invariant (First_Col in Grid'Range (2));
         Get_Last_Col_of_Problem (Grid, First_Col, Last_Col);
         if Part = Part_1 then
            Get_Operands_Part1 (Grid, First_Col, Last_Col, Operands);
         else
            Get_Operands_Part2 (Grid, First_Col, Last_Col, Operands);
         end if;
         if Grid (Grid'Last (1), First_Col) = '*' then
            Result := 1;
            for C in Operands loop
               Result :=
                  Result * To_Big_Integer (Element (Operands, C));
            end loop;
         else
            Result := 0;
            for C in Operands loop
               Result :=
                  Result + To_Big_Integer (Element (Operands, C));
            end loop;
         end if;
         Total_Grand := Total_Grand + Result;
         exit when Last_Col = Grid'Last (2);
         First_Col := Last_Col + 2;
      end loop;
      Put ("Total grand:"); Put_Line (To_String (Total_Grand));
   exception
      when Invalid_Input => null;
   end Process_Worksheet;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Input   : Generic_Input_Type;
      Success : Boolean;

   begin
      Read_Worksheet (Filename, Success, Input);
      if Success then
         declare
            Grid : constant Grid_Type := Create_Grid_From_Input (Input);
         begin
            if not Is_Empty (Grid) then
               if Grid'Length (1) >= 2 then
                  Process_Worksheet (Part, Grid);
               else
                  Put_Line ("Error: Input has only one line.");
               end if;
            else
               Put_Line ("Error: Couldn't create the grid.");
            end if;
         end;
      end if;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day06/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day06/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day06/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day06/input.txt");
end Day06;