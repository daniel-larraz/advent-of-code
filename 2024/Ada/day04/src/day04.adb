pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day04 is

   package Input_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => String);

   use Input_Pkg;

   subtype Input_Type is Input_Pkg.Vector;

   type Table_Type is
      array (Positive range <>, Positive range <>) of Character;

   procedure Read_Line (
      Line  : String;
      Part  : Puzzle_Part;
      Input : in out Input_Type
   ) with
      Exceptional_Cases => (Invalid_Input => True)
   is
   begin
      if Part = Part_1 or else Part = Part_2 then
         Check (Length (Input) < Last_Count, "Input length validation failed");
         Append (Input, Line);
      end if;
   end Read_Line;

   function Invariant (UNUSED : Input_Type) return Boolean is (True);

   procedure Read_Input is new Process_File_Line_by_Line (
      Data_Type => Input_Type,
      Invariant => Invariant,
      Process_Line => Read_Line
   );

   function Create_Table (
      Input : Input_Type; Num_Rows, Num_Cols : Natural
   ) return Table_Type
   with
      Pre => Num_Rows > 0 and then Num_Cols > 0 and then
         Num_Rows = Natural (Length (Input)) and then
         (for all I in Input =>
            Str_Length (Element (Input, I)) = Num_Cols)
   is
   begin
      declare
         Table : Table_Type := [1 .. Num_Rows => [1 .. Num_Cols => '?']];
      begin
         for I in Input loop
            declare
               Line : String renames Element (Input, I);
            begin
               for J in Line'Range loop
                  Table (I, J - Line'First + 1) := Line (J);
               end loop;
            end;
         end loop;
         return Table;
      end;
   end Create_Table;

   procedure Compute_Dimensions (
      Input    : Input_Type;
      Num_Rows : out Natural;
      Num_Cols : out Natural
   )
   with
      Post => Num_Rows > 0 and then Num_Cols > 0 and then
         Num_Rows = Natural (Length (Input)) and then
         (for all I in Input =>
            Str_Length (Element (Input, I)) = Num_Cols),

      Exceptional_Cases => (Invalid_Input => True)
   is
   begin
      Num_Rows := Natural (Length (Input));
      Check (Num_Rows > 0, "No rows");
      Num_Cols := Str_Length (Element (Input, First_Index (Input)));
      Check (Num_Cols > 0, "No columns");
      for I in First_Index (Input) + 1 .. Last_Index (Input) loop
         pragma Loop_Invariant (for all J in First_Index (Input) .. I - 1 =>
            Str_Length (Element (Input, J)) = Num_Cols);
         Check (Str_Length (Element (Input, I)) = Num_Cols,
                  "Different number of columns");
      end loop;
   end Compute_Dimensions;

   procedure Count_XMAS (
      Part : Puzzle_Part;
      Table : Table_Type;
      N : out Big_Natural
   )
   is

      function In_Integer (I1, I2 : Integer) return Boolean is
      (if I2 >= 0 then I1 <= Integer'Last - I2 else I1 >= Integer'First - I2);

      function Ck (I1, I2, J1, J2 : Integer; C : Character) return Boolean is
      (In_Integer (I1, I2) and then (I1 + I2) in Table'Range (1) and then
       In_Integer (J1, J2) and then (J1 + J2) in Table'Range (2) and then
       Table (I1 + I2, J1 + J2) = C);

      procedure Count_Part_1 (R, C : Integer) is
      begin
         if Table (R, C) = 'X' then
            --  North
            if Ck (R, -1, C, 0, 'M') and then Ck (R, -2, C, 0, 'A') and then
               Ck (R, -3, C, 0, 'S')
            then
               N := N + 1;
            end if;
            --  North East
            if Ck (R, -1, C, 1, 'M') and then Ck (R, -2, C, 2, 'A') and then
               Ck (R, -3, C, 3, 'S')
            then
               N := N + 1;
            end if;
            --  East
            if Ck (R, 0, C, 1, 'M') and then Ck (R, 0, C, 2, 'A') and then
               Ck (R, 0, C, 3, 'S')
            then
               N := N + 1;
            end if;
            --  South East
            if Ck (R, 1, C, 1, 'M') and then Ck (R, 2, C, 2, 'A') and then
               Ck (R, 3, C, 3, 'S')
            then
               N := N + 1;
            end if;
            --  South
            if Ck (R, 1, C, 0, 'M') and then Ck (R, 2, C, 0, 'A') and then
               Ck (R, 3, C, 0, 'S')
            then
               N := N + 1;
            end if;
            --  South West
            if Ck (R, 1, C, -1, 'M') and then Ck (R, 2, C, -2, 'A') and then
               Ck (R, 3, C, -3, 'S')
            then
               N := N + 1;
            end if;
            --  West
            if Ck (R, 0, C, -1, 'M') and then Ck (R, 0, C, -2, 'A') and then
               Ck (R, 0, C, -3, 'S')
            then
               N := N + 1;
            end if;
            --  North West
            if Ck (R, -1, C, -1, 'M') and then Ck (R, -2, C, -2, 'A') and then
               Ck (R, -3, C, -3, 'S')
            then
               N := N + 1;
            end if;
         end if;
      end Count_Part_1;

      procedure Count_Part_2 (R, C : Integer) is
      begin
         if Table (R, C) = 'A' then
            if ((Ck (R, -1, C, -1, 'M') and then Ck (R, 1, C, 1, 'S')) or else
                (Ck (R, -1, C, -1, 'S') and then Ck (R, 1, C, 1, 'M')))
               and then
               ((Ck (R, 1, C, -1, 'M') and then Ck (R, -1, C, 1, 'S')) or else
                (Ck (R, 1, C, -1, 'S') and then Ck (R, -1, C, 1, 'M')))
            then
               N := N + 1;
            end if;
         end if;
      end Count_Part_2;

   begin
      N := 0;
      for R in Table'Range (1) loop
         pragma Loop_Invariant (N >= 0);
         for C in Table'Range (2) loop
            pragma Loop_Invariant (N >= 0);
            if Part = Part_1 then
               Count_Part_1 (R, C);
            else
               Count_Part_2 (R, C);
            end if;
         end loop;
      end loop;
   end Count_XMAS;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success  : Boolean;
      Input    : Input_Type;
      Num_Rows : Natural;
      Num_Cols : Natural;
      Result   : Big_Natural;
   begin
      Read_Input (Filename, Part, Success, Input);
      if Success then
         Compute_Dimensions (Input, Num_Rows, Num_Cols);
         declare
            Table : constant Table_Type :=
               Create_Table (Input, Num_Rows, Num_Cols);
         begin
            Count_XMAS (Part, Table, Result);
            Put ("Result:"); Put_Line (To_String (Result));
         end;
      end if;
   exception
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day04/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day04/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day04/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day04/input.txt");
end Day04;