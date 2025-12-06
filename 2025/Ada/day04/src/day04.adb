pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

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
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      if Part = Part_1 or else Part = Part_2 then
         Check (Length (Input) < Last_Count,
            "Input length validation failed.");
         Check (Line'Length < Natural'Last - 1,
            "Line length validation failed.");
         declare
            Ext_Line : String (1 .. Line'Length + 2) := [others => '.'];
         begin
            Ext_Line (2 .. Ext_Line'Last - 1) := Line;
            Append (Input, Ext_Line);
         end;
      end if;
   end Read_Line;

   function Invariant (UNUSED : Input_Type) return Boolean is (True);

   procedure Read_Input is new Process_File_Line_by_Line (
      Data_Type => Input_Type,
      Invariant => Invariant,
      Process_Line => Read_Line
   );

   procedure Compute_and_Check_Dimensions (
      Input    : Input_Type;
      Num_Rows : out Natural;
      Num_Cols : out Natural
   )
   with
      Post => Num_Rows > 0 and then Num_Cols >= 3 and then
         Num_Rows = Natural (Length (Input)) and then
         (for all I in Input =>
            Str_Length (Element (Input, I)) = Num_Cols),

      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      Num_Rows := Natural (Length (Input));
      Check (Num_Rows > 0, "No rows.");
      Num_Cols := Str_Length (Element (Input, First_Index (Input)));
      Check (Num_Cols >= 3, "Row has fewer than 3 columns.");
      for I in First_Index (Input) + 1 .. Last_Index (Input) loop
         pragma Loop_Invariant (for all J in First_Index (Input) .. I - 1 =>
            Str_Length (Element (Input, J)) = Num_Cols);
         Check (Str_Length (Element (Input, I)) = Num_Cols,
                  "Different number of columns.");
      end loop;
   end Compute_and_Check_Dimensions;

   procedure Add_Sentinels (
      Num_Cols : Natural;
      Input    : in out Input_Type
   )
   with
      Pre => (for all I in Input =>
         Str_Length (Element (Input, I)) = Num_Cols),
      Post => Length (Input) = Length (Input'Old) + 2 and then
         (for all I in Input =>
            Str_Length (Element (Input, I)) = Num_Cols),
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Last : constant Natural := Num_Cols;
      Sentinel : constant String (1 .. Last) := [others => '.'];
   begin
      Check (Length (Input) < Last_Count,
         "Input length validation failed.");
      Prepend (Input, Sentinel);
      Check (Length (Input) < Last_Count,
         "Input length validation failed.");
      Append (Input, Sentinel);
   end Add_Sentinels;

   function Create_Table (
      Input : Input_Type; Num_Rows, Num_Cols : Natural
   ) return Table_Type
   with
      Pre => Num_Rows >= 3 and then Num_Cols >= 3 and then
         Num_Rows = Natural (Length (Input)) and then
         (for all I in Input =>
            Str_Length (Element (Input, I)) = Num_Cols),
      Post => Create_Table'Result'First (1) = 1  and then
              Create_Table'Result'Last  (1) >= 3 and then
              Create_Table'Result'First (2) = 1  and then
              Create_Table'Result'Last  (2) >= 3
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

   function IsAccesible (
      Table : Table_Type;
      R     : Natural;
      C     : Natural) return Boolean
   with
      Pre => Table'Length (1) >= 3 and then Table'Length (2) >= 3 and then
             Table'First (1) < R and then  R < Table'Last (1) and then
             Table'First (2) < C and then C < Table'Last (2)
   is
      Num_Rolls  : Natural := 0;
      Processed : Natural := 0 with Ghost;
   begin
      for I in -1 .. 1 loop
         for J in -1 .. 1 loop
            pragma Loop_Invariant (Processed = 3 * (I + 1) + (J + 1));
            pragma Loop_Invariant (Num_Rolls <= Processed);
            Processed := Processed + 1;
            if not (I = 0 and then J = 0) then
               if Table (R + I, C + J) = '@' then
                  Num_Rolls := Num_Rolls + 1;
               end if;
            end if;
         end loop;
      end loop;
      return Num_Rolls < 4;
   end IsAccesible;

   procedure Process_Table_Part1 (
      Table  : Table_Type;
      Result : in out Big_Natural)
   is
   begin
      for R in Table'First (1) + 1 .. Table'Last (1) - 1 loop
         for C in Table'First (2) + 1 .. Table'Last (2) - 1 loop
            if Table (R, C) = '@' then
               if IsAccesible (Table, R, C) then
                  Result := Result + 1;
               end if;
            end if;
         end loop;
      end loop;
   end Process_Table_Part1;

   procedure Process_Table_Part2 (
      Table  : in out Table_Type;
      Result : in out Big_Natural)
   is
      Changed : Boolean := True;
   begin
      while Changed loop
         Changed := False;
         for R in Table'First (1) + 1 .. Table'Last (1) - 1 loop
            for C in Table'First (2) + 1 .. Table'Last (2) - 1 loop
               if Table (R, C) = '@' then
                  if IsAccesible (Table, R, C) then
                     Table (R, C) := '.';
                     Changed := True;
                     Result := Result + 1;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
   end Process_Table_Part2;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success  : Boolean;
      Input    : Input_Type;
      Num_Rows : Natural;
      Num_Cols : Natural;
   begin
      Read_Input (Filename, Part, Success, Input);
      if Success then
         Compute_and_Check_Dimensions (Input, Num_Rows, Num_Cols);
         Add_Sentinels (Num_Cols, Input);
         declare
            Result : Big_Natural := 0;
            Table  : Table_Type :=
               Create_Table (Input, Num_Rows + 2, Num_Cols);
         begin
            if Part = Part_1 then
               Process_Table_Part1 (Table, Result);
            else
               Process_Table_Part2 (Table, Result);
            end if;
            Put ("Number of rolls:"); Put_Line (To_String (Result));
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