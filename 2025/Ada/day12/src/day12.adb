pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;

procedure Day12 is

   N : constant Positive := 5;

   type Big_Int_Array is array (0 .. N) of Big_Integer;

   type Puzzle_Info is record
      Current_Id        : Integer := -1;
      Num_Shape_Blocks  : Big_Int_Array := [others => 0];
      Valid_Regions     : Big_Integer := 0;
   end record;

   type Region_Info is record
      Current_Id : Natural;
      Counts     : Big_Int_Array;
   end record;

   Initial_Region_Info : constant Region_Info := (0, [others => 0]);

   procedure Read_Counts (Line : String; Reg_Info : in out Region_Info)
   with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      Check (Reg_Info.Current_Id in Reg_Info.Counts'Range,
         "More count values than shapes.");
      Read_Big_Natural (Line, Reg_Info.Counts (Reg_Info.Current_Id));
      Check (Reg_Info.Current_Id <= N, "Maximum number of counts exceeded.");
      Reg_Info.Current_Id := Reg_Info.Current_Id + 1;
   end Read_Counts;

   procedure Read_Region_Info is new Fold_Delimited_String (
      Accumulator_Type => Region_Info,
      Initial_Accumulator_Value => Initial_Region_Info,
      Separator => ' ',
      Process_Substring => Read_Counts
   );

   function Invariant (UNUSED : Puzzle_Info) return Boolean is (True);

   procedure Read_Line (
      Line   : String;
      UNUSED : Puzzle_Part;
      Info   : in out Puzzle_Info
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      Idx1, Idx2 : Natural;
      Width, Height : Big_Integer;
   begin
      if Line'Length > 0 then
         if Line (Line'Last) = ':' then
            Check (Info.Current_Id < N, "Maximum number of shapes exceeded.");
            Info.Current_Id := Info.Current_Id + 1;
         elsif Line (Line'First) = '.' or else Line (Line'First) = '#' then
            for C of Line loop
               if C = '#' then
                  Check (Info.Current_Id in Info.Num_Shape_Blocks'Range,
                     "Maximum number of shapes exceeded.");
                  Info.Num_Shape_Blocks (Info.Current_Id) :=
                     Info.Num_Shape_Blocks (Info.Current_Id) + 1;
               end if;
            end loop;
         else
            Idx1 := Index (Line, 'x');
            Check (Line'First <= Idx1 and then Idx1 <= Line'Last - 1,
               "Invalid line format.");
            Read_Big_Natural (Line (Line'First .. Idx1 - 1), Width);
            Idx2 := Index (Line (Idx1 + 1 .. Line'Last), ':');
            Check (Idx1 + 1 < Idx2 and then Idx2 <= Line'Last - 2,
               "Invalid line format.");
            Read_Big_Natural (Line (Idx1 + 1 .. Idx2 - 1), Height);
            declare
               Reg_Info   : Region_Info;
               Num_Blocks : Big_Integer := 0;
            begin
               Read_Region_Info (
                  Line (Idx2 + 2 .. Line'Last), Reg_Info);
               for I in Info.Num_Shape_Blocks'Range loop
                  Num_Blocks := Num_Blocks +
                     Info.Num_Shape_Blocks (I) * Reg_Info.Counts (I);
               end loop;
               if Num_Blocks <= Width * Height then
                  Info.Valid_Regions := Info.Valid_Regions + 1;
               end if;
            end;
         end if;
      end if;
   end Read_Line;

   procedure Process_Input is new Process_File_Line_by_Line (
      Data_Type => Puzzle_Info,
      Invariant => Invariant,
      Process_Line => Read_Line
   );

   procedure Process_File (Filename : String) is
      Success   : Boolean;
      Info      : Puzzle_Info;
   begin
      Process_Input (Filename, Part_1, Success, Info);
      if Success then
         Put ("Valid regions:");
         Put_Line (To_String (Info.Valid_Regions));
      end if;
   exception
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Input - Part 1 #");
   Process_File ("./share/day12/input.txt");
end Day12;