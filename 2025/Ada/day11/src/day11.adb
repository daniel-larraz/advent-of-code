pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;
with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

procedure Day11 is

   subtype Name is String (1 .. 3);

   package Name_List_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Name);

   use Name_List_Pkg;

   subtype Name_List is Name_List_Pkg.Vector;

   package Name_Graph_Pkg is new
      SPARK.Containers.Formal.Unbounded_Ordered_Maps (
         Key_Type => Name, Element_Type => Name_List);

   use Name_Graph_Pkg;

   subtype Name_Graph is Name_Graph_Pkg.Map;

   procedure Read_Name (N : String; L : in out Name_List)
   with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
   begin
      Check (N'Length = 3, "Name must have 3 characters.");
      Check (Length (L) < Name_List_Pkg.Last_Count,
         "Vector length validation failed."
      );
      Append (L, N);
   end Read_Name;

   procedure Read_Names is new Fold_Delimited_String (
      Accumulator_Type => Name_List,
      Initial_Accumulator_Value => Empty_Vector,
      Separator => ' ',
      Process_Substring => Read_Name
   );

   function Invariant (UNUSED : Name_Graph) return Boolean is (True);

   procedure Read_Line (
      Line   : String;
      UNUSED : Puzzle_Part;
      G      : in out Name_Graph
   ) with
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      L   : Name_List;
   begin
      Check (Line'Length >= 8,
         "Line has fewer characters than the minimum expected.");
      declare
         N : constant Name := Line (Line'First .. Line'First + 2);
      begin
         Read_Names (Line (Line'First + 5 .. Line'Last), L);
         Check (not Contains (G, N),
            "The device name is already in the map.");
         Check (Length (G) < Last_Count, "Map length validation failed.");
         Insert (G, N, L);
      end;
   end Read_Line;

   procedure Read_Input is new Process_File_Line_by_Line (
      Data_Type => Name_Graph,
      Invariant => Invariant,
      Process_Line => Read_Line
   );

   procedure Count_Paths (
      Part      : Puzzle_Part;
      G         : Name_Graph;
      Num_Paths : out Big_Integer)
   with
      Exceptional_Cases => (Invalid_Input => True)
   is
      subtype Key is String (1 .. 5);

      package Count_Map_Pkg is new
         SPARK.Containers.Formal.Unbounded_Ordered_Maps (
            Key_Type => Key, Element_Type => Big_Integer);

      use Count_Map_Pkg;

      subtype Count_Map is Count_Map_Pkg.Map;

      Memo : Count_Map;

      function Make_Key (N : Name; D, F : Boolean) return Key
      is
      begin
         return N & (if D then "1" else "0") &
            (if F then "1" else "0");
      end Make_Key;

      procedure Count_Paths (
         N        : Name;
         Seen_DAC : Boolean;
         Seen_FFT : Boolean;
         Sum      : out Big_Integer
      )
      with
         Exceptional_Cases => (Invalid_Input => True)
      is
         D : constant Boolean := Seen_DAC or else N = "dac";
         F : constant Boolean := Seen_FFT or else N = "fft";
         K : constant Key := Make_Key (N, D, F);

         MC  : Count_Map_Pkg.Cursor;
         GC  : Name_Graph_Pkg.Cursor;
      begin
         Sum := 0;

         MC := Find (Memo, K);
         if MC /= Count_Map_Pkg.No_Element then
            Sum := Element (Memo, MC);
            return;
         end if;

         if N = "out" then
            if D and F then
               Sum := 1;
            end if;
            return;
         end if;

         GC := Find (G, N);
         if GC = Name_Graph_Pkg.No_Element then
            return;
         end if;

         declare
            Children renames Element (G, GC);
            Child_Sum : Big_Integer;
         begin
            for C of Children loop
               Count_Paths (C, D, F, Child_Sum);
               Sum := Sum + Child_Sum;
            end loop;
         end;
         Check (not Contains (Memo, K), "Cycle detected.");
         Check (Length (Memo) < Last_Count, "Map length validation failed.");
         Insert (Memo, K, Sum);
      end Count_Paths;

   begin
      if Part = Part_1 then
         Count_Paths ("you", True, True, Num_Paths);
      else
         Count_Paths ("svr", False, False, Num_Paths);
      end if;
      pragma Assert (Length (Memo) >= 0);
   end Count_Paths;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success   : Boolean;
      G         : Name_Graph;
      Num_Paths : Big_Integer;
   begin
      Read_Input (Filename, Part, Success, G);
      if Success then
         Count_Paths (Part, G, Num_Paths);
         Put ("Number of paths: ");
         Put_Line (To_String (Num_Paths));
      end if;
   exception
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day11/example1.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day11/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day11/example2.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day11/input.txt");
end Day11;