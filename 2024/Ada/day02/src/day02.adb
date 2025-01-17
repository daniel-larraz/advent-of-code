pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day02 is

   package Report_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Natural, Element_Type => Big_Natural);

   use Report_Pkg;

   subtype Report_Type is Report_Pkg.Vector;

   procedure Read_Level (
      Line   : String;
      Report : in out Report_Type
   )
   with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Level : Big_Natural;
   begin
      Read_Big_Natural (Line, Level);
      Check (Length (Report) < Last_Count, "Report length validation failed");
      Append (Report, Level);
   end Read_Level;

   procedure Read_Report is new Fold_Delimited_String (
      Accumulator_Type => Report_Type,
      Initial_Accumulator_Value => Empty_Vector,
      Separator => ' ',
      Process_Substring => Read_Level
   );

   generic
      Factor : Big_Integer;
   function Is_Diff_Within_Bounds (Report : Report_Type) return Boolean;

   function Is_Diff_Within_Bounds (Report : Report_Type) return Boolean is
   begin
      for I in First_Index (Report) + 1 .. Last_Index (Report) loop
         declare
            Diff : constant Big_Integer := Factor *
               (Element (Report, I) - Element (Report, I - 1));
         begin
            if Diff < 1 or else Diff > 3 then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_Diff_Within_Bounds;

   function Is_Increasing_Within_Bounds is
      new Is_Diff_Within_Bounds (Factor => 1);

   function Is_Decreasing_Within_Bounds is
      new Is_Diff_Within_Bounds (Factor => -1);

   function Is_Safe (Report : Report_Type) return Boolean is
   begin
      return Is_Increasing_Within_Bounds (Report) or else
             Is_Decreasing_Within_Bounds (Report);
   end Is_Safe;

   function Is_Dampener_Safe (Report : Report_Type) return Boolean is
   begin
      for I in Report loop
         declare
            D_Report : Report_Type := Report;
         begin
            Delete (D_Report, I);
            if Is_Safe (D_Report) then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Dampener_Safe;

   procedure Process_Report (
      Line     : String;
      Part     : Puzzle_Part;
      Num_Safe : in out Natural
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Report : Report_Type;
   begin
      Read_Report (Line, Report);
      if Is_Safe (Report) or else
         (Part = Part_2 and then Is_Dampener_Safe (Report))
      then
         Check (
            Num_Safe < Natural'Last, "Overflow while computing Num_Safe"
         );
         Num_Safe := Num_Safe + 1;
      end if;
   end Process_Report;

   function Invariant (UNUSED : Natural) return Boolean is (True);

   procedure Process_Reports is new Process_File_Line_by_Line (
      Data_Type => Natural,
      Invariant => Invariant,
      Process_Line => Process_Report
   );

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success  : Boolean;
      Num_Safe : Natural := 0;
   begin
      Process_Reports (Filename, Part, Success, Num_Safe);
      if Success then
         Put_Line ("Safe reports:" & Num_Safe'Image);
      end if;
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