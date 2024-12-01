pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;
with SPARK.Containers.Formal.Unbounded_Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day01 is

   package Location_List_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Natural, Element_Type => Big_Natural);

   package Location_List_Sorting is
      new Location_List_Pkg.Generic_Sorting;

   package Location_Map_Pkg is new
      SPARK.Containers.Formal.Unbounded_Ordered_Maps
      (Key_Type => Big_Natural, Element_Type => Big_Natural);

   use Location_List_Pkg;
   use Location_Map_Pkg;
   use Location_List_Sorting;

   subtype Location_List is Location_List_Pkg.Vector;
   subtype Location_Map is Location_Map_Pkg.Map;

   type Input_Type is record
      Left_List  : Location_List;
      Right_List : Location_List;
   end record;

   procedure Read_Id_Pair (
      Line  : String;
      Part  : Puzzle_Part;
      Input : in out Input_Type
   ) with
      Pre => Length (Input.Left_List) = Length (Input.Right_List),
      Post => Length (Input.Left_List) = Length (Input.Right_List),
      Exceptional_Cases => (Invalid_Input => True)
   is
      Id  : Big_Natural;
      Idx : Natural;
   begin
      if Part = Part_1 or else Part = Part_2 then
         Idx := Index (Line, ' ');
         Check (
            Line'First < Idx and then Idx < Line'Last,
            "Invalid line format (Left)"
         );
         Read_Big_Natural (Line (Line'First .. Idx - 1), Id);
         Check (
            Length (Input.Left_List) < Last_Count,
            "Input.Left_List length validation failed"
         );
         Append (Input.Left_List, Id);
         Idx := Rev_Index (Line, ' ');
         Check (
            Line'First < Idx and then Idx < Line'Last,
            "Invalid line format (Right)"
         );
         Read_Big_Natural (Line (Idx + 1 .. Line'Last), Id);
         Check (
            Length (Input.Right_List) < Last_Count,
            "Input.Right_List length validation failed"
         );
         Append (Input.Right_List, Id);
      end if;
   end Read_Id_Pair;

   function Equal_Length (Input : Input_Type) return Boolean is
      (Length (Input.Left_List) = Length (Input.Right_List));

   procedure Read_Location_Ids is new Process_File_Line_by_Line (
      Data_Type => Input_Type,
      Invariant => Equal_Length,
      Process_Line => Read_Id_Pair
   );

   procedure Process_File_Part1 (Filename : String) is
      Success      : Boolean;
      Input        : Input_Type;
      Left_Lst     : Location_List renames Input.Left_List;
      Right_Lst    : Location_List renames Input.Right_List;
      Distance_Sum : Big_Natural := 0;
   begin
      Read_Location_Ids (Filename, Part_1, Success, Input);
      if Success then
         Sort (Left_Lst);
         Sort (Right_Lst);
         for Index in First_Index (Left_Lst) .. Last_Index (Left_Lst) loop
            Distance_Sum := Distance_Sum +
               abs (Element (Left_Lst, Index) - Element (Right_Lst, Index));
         end loop;
         Put ("Total distance:"); Put_Line (To_String (Distance_Sum));
      end if;
   exception
      when Invalid_Input => null;
   end Process_File_Part1;

   procedure Compute_Frequency_Map (
      List : Location_List;
      Map  : out Location_Map
   ) with
      Exceptional_Cases => (Invalid_Input => True)
   is
   begin
      Map := Empty_Map;
      for Idx in List loop
         declare
            E : Big_Natural renames Element (List, Idx);
            C : constant Cursor := Find (Map, E);
         begin
            if C /= No_Element then
               Replace (Map, E, Element (Map, C) + 1);
            else
               Check (
                  Length (Map) < Last_Count,
                  "Map length validation failed"
               );
               Insert (Map, E, 1);
            end if;
         end;
      end loop;
   end Compute_Frequency_Map;

   procedure Process_File_Part2 (Filename : String) is
      Success          : Boolean;
      Input            : Input_Type;
      Left_Lst         : Location_List renames Input.Left_List;
      Right_Lst        : Location_List renames Input.Right_List;
      Map              : Location_Map;
      Similarity_Score : Big_Natural := 0;
   begin
      Read_Location_Ids (Filename, Part_2, Success, Input);
      if Success then
         Compute_Frequency_Map (Right_Lst, Map);
         --  Compute similarity score
         for Idx in Left_Lst loop
            declare
               E : Big_Natural renames Element (Left_Lst, Idx);
               C : constant Cursor := Find (Map, E);
               Count : Big_Natural := 0;
            begin
               if C /= No_Element then
                  Count := Element (Map, C);
               end if;
               Similarity_Score := Similarity_Score +
                  Element (Left_Lst, Idx) * Count;
            end;
         end loop;
         Put ("Similarity score:");
         Put_Line (To_String (Similarity_Score));
      end if;
   exception
      when Invalid_Input => null;
   end Process_File_Part2;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File_Part1 ("./share/day01/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File_Part1 ("./share/day01/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File_Part2 ("./share/day01/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File_Part2 ("./share/day01/input.txt");
end Day01;