pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day03 is

   function Invariant (UNUSED : Big_Integer) return Boolean is (True);

   procedure Process_Bank_Part1 (
      Line          : String;
      Total_Joltage : in out Big_Integer
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Max_Joltage : Big_Integer := 0;
      Current     : Big_Integer;
   begin
      Check (Line'Length > 0, "Found an empty line.");
      for I in Line'First .. Line'Last - 1 loop
         for J in I + 1 .. Line'Last loop
            Check (Line (I) in '0' .. '9', "Character is not a digit.");
            Check (Line (J) in '0' .. '9', "Character is not a digit.");
            Current := To_Big_Integer (To_Digit (Line (I))) * 10
               + To_Big_Integer (To_Digit (Line (J)));
            if Current > Max_Joltage then
               Max_Joltage := Current;
            end if;
         end loop;
      end loop;
      Total_Joltage := Total_Joltage + Max_Joltage;
   end Process_Bank_Part1;

   procedure Process_Bank_Part2 (
      Line          : String;
      Total_Joltage : in out Big_Integer
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Num_Digits : constant Natural := 12;
      Result     : String (1 .. Num_Digits);
      Start      : Integer := Line'First;
   begin
      Check (Line'Length >= Num_Digits,
         "Line has fewer than " & Num_Digits'Image & " digits.");
      for Pos in 1 .. Num_Digits loop
         pragma Loop_Invariant (Start in Line'Range);
         declare
            Max_Char : Character := '0';
            Max_Pos  : Integer := Start;
         begin
            for I in Start .. Line'Last - (Num_Digits - Pos) loop
               pragma Loop_Invariant (I in Line'Range);
               pragma Loop_Invariant (Max_Pos in Start .. I);
               if Line (I) > Max_Char then
                  Max_Char := Line (I);
                  Max_Pos := I;
               end if;
            end loop;
            Result (Pos) := Max_Char;
            if Max_Pos < Line'Last then
               Start := Max_Pos + 1;
            end if;
         end;
      end loop;
      declare
         Max_Joltage : Big_Natural;
      begin
         Read_Big_Natural (Result, Max_Joltage);
         Total_Joltage := Total_Joltage + Max_Joltage;
      end;
   end Process_Bank_Part2;

   procedure Process_Bank (
      Line          : String;
      Part          : Puzzle_Part;
      Total_Joltage : in out Big_Integer
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      if Part = Part_1 then
         Process_Bank_Part1 (Line, Total_Joltage);
      else
         Process_Bank_Part2 (Line, Total_Joltage);
      end if;
   end Process_Bank;

   procedure Process_Banks is new Process_File_Line_by_Line (
      Data_Type => Big_Integer,
      Invariant => Invariant,
      Process_Line => Process_Bank
   );

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success       : Boolean;
      Total_Joltage : Big_Integer := 0;
   begin
      Process_Banks (Filename, Part, Success, Total_Joltage);
      if Success then
         Put ("Total output joltage:");
         Put_Line (To_String (Total_Joltage));
      end if;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day03/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day03/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day03/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day03/input.txt");
end Day03;