pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;

procedure Day07 is

   Max_Beams : constant Positive := 150;

   type Unconstrained_Presence_Map is array (Natural range <>) of Boolean;
   type Unconstrained_Count_Map is array (Natural range <>) of Big_Integer;

   subtype Presence_Map is Unconstrained_Presence_Map (0 .. Max_Beams + 1);
   subtype Count_Map is Unconstrained_Count_Map (0 .. Max_Beams + 1);

   type Puzzle_Info is record
      Is_Present : Presence_Map := [others => False];
      Counter    : Count_Map := [others => 0];
      Num_Splits : Big_Integer := 0;
   end record;

   procedure Read_Line (
      Line : String;
      Part : Puzzle_Part;
      Info : in out Puzzle_Info
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      Check (Line'First = 1 and then Line'Last < Max_Beams,
         "Line and Map bounds are incompatible.");

      for I in Line'Range loop
         if Line (I) = 'S' then
            Info.Is_Present (I) := True;
            if Part = Part_2 then
               Info.Counter (I) := 1;
            end if;
         elsif Line (I) = '^' then
            if Info.Is_Present (I) then
               Info.Is_Present (I - 1) := True;
               Info.Is_Present (I + 1) := True;
               Info.Is_Present (I) := False;
               Info.Num_Splits := Info.Num_Splits + 1;
               if Part = Part_2 then
                  Info.Counter (I - 1) :=
                     Info.Counter (I - 1) + Info.Counter (I);
                  Info.Counter (I + 1) :=
                     Info.Counter (I + 1) + Info.Counter (I);
                  Info.Counter (I) := 0;
               end if;
            end if;
         end if;
      end loop;
   end Read_Line;

   function Invariant (UNUSED : Puzzle_Info) return Boolean is (True);

   procedure Read_Input is new Process_File_Line_by_Line (
      Max_Line_Length => Max_Beams,
      Data_Type => Puzzle_Info,
      Invariant => Invariant,
      Process_Line => Read_Line
   );

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success : Boolean;
      Info    : Puzzle_Info;
   begin
      Read_Input (Filename, Part, Success, Info);
      if Success then
         if Part = Part_2 then
            Info.Num_Splits := 0;
            for I in Info.Counter'Range loop
               Info.Num_Splits := Info.Num_Splits + Info.Counter (I);
            end loop;
         end if;
         Put ("Answer:");
         Put_Line (To_String (Info.Num_Splits));
      end if;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day07/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day07/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day07/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day07/input.txt");
end Day07;