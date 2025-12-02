pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day01 is

   type Dial_Number is mod 100;

   M : constant Natural := Dial_Number'Modulus;

   MAX_DISTANCE : constant Natural := 1000; -- Maximum allowed distance

   type Puzzle_Info is record
     Position : Dial_Number := 50;
     Password : Big_Natural := 0;
   end record;

   function Invariant (UNUSED : Puzzle_Info) return Boolean is (True);

   procedure Read_Rotation (
      Line : String;
      Part : Puzzle_Part;
      Info : in out Puzzle_Info
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Direction : Character;
      Distance  : Natural;
   begin
      Check (Line'Length >= 2, "A line must have at least two characters");

      Direction := Line (Line'First);
      Check (Direction = 'L' or else Direction = 'R',
         "The direction of a rotation should be 'L' or 'R', but found "
         & Direction);

      Read_Natural (
         Line (Line'First + 1 .. Line'Last), Distance, MAX_DISTANCE);

      if Part = Part_1 then
         if Direction = 'L' then
            Info.Position := Info.Position - Dial_Number'Mod (Distance);
         else
            Info.Position := Info.Position + Dial_Number'Mod (Distance);
         end if;
         if Info.Position = 0 then
            Info.Password := Info.Password + 1;
         end if;
      else
         declare
            Diff, Wraps : Integer;
            P : constant Natural := Natural (Info.Position);
         begin
            if Direction = 'L' then
               Diff := P - Distance;
               if Diff <= 0 then
                  Wraps := Diff / (-M) + (if P = 0 then 0 else 1);
                  Info.Password := Info.Password + To_Big_Integer (Wraps);
               end if;
               Info.Position := Info.Position - Dial_Number'Mod (Distance);
            else
               Wraps := (P + Distance) / M;
               Info.Password := Info.Password + To_Big_Integer (Wraps);
               Info.Position := Info.Position + Dial_Number'Mod (Distance);
            end if;
         end;
      end if;

   end Read_Rotation;

   procedure Read_Rotations is new Process_File_Line_by_Line (
      Data_Type => Puzzle_Info,
      Invariant => Invariant,
      Process_Line => Read_Rotation
   );

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success      : Boolean;
      Info         : Puzzle_Info;
   begin
      Read_Rotations (Filename, Part, Success, Info);
      if Success then
         Put ("Password:"); Put_Line (To_String (Info.Password));
      end if;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day01/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day01/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day01/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day01/input.txt");
end Day01;
