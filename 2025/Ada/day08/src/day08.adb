pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

procedure Day08 is

   type Point is record
      X, Y, Z : Integer;
   end record;

   package Points_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Point);

   use Points_Pkg;

   subtype Point_List is Points_Pkg.Vector;

   type Edge is record
      A, B : Positive;
      Dist2  : Big_Integer;
   end record;

   function "<" (E1 : Edge; E2 : Edge) return Boolean is (E1.Dist2 < E2.Dist2);

   package Edges_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Edge);

   package Edges_Sorting is new Edges_Pkg.Generic_Sorting;

   use Edges_Sorting;

   subtype Edge_List is Edges_Pkg.Vector;

   function Are_Edges_Valid (Points : Point_List; Edges : Edge_List)
      return Boolean
   is
      (for all C in Edges =>
         (declare
            E : Edge renames Edges_Pkg.Element (Edges, C);
         begin
            E.A in First_Index (Points) .. Last_Index (Points) and then
            E.B in First_Index (Points) .. Last_Index (Points)
         )
      )
      with Ghost;

   type Pos_Array is array (Positive range <>) of Positive;

   type Disjoint_Set (N : Positive) is record
      Parent   : Pos_Array (1 .. N) := [for I in 1 .. N => I];
      Size     : Pos_Array (1 .. N) := [others => 1];
      Num_Sets : Positive := N;
   end record;

   function Parents_In_Range (S : Disjoint_Set) return Boolean is
      (for all I in S.Parent'Range => S.Parent (I) in S.Parent'Range)
      with Ghost;

   procedure Find (
      S : in out Disjoint_Set;
      X : Positive;
      R : out Positive)
   with
      Pre => Parents_In_Range (S) and then X in S.Parent'Range,
      Post => Parents_In_Range (S) and then R in S.Parent'Range
   is
   begin
      if S.Parent (X) /= X then
         declare
            Root : Positive;
         begin
            Find (S, S.Parent (X), Root);
            S.Parent (X) := Root;
         end;
      end if;
      R := S.Parent (X);
   end Find;

   procedure Union (
      S : in out Disjoint_Set;
      A : Positive;
      B : Positive
   )
   with
      Pre => Parents_In_Range (S) and then
         A in S.Parent'Range and then B in S.Parent'Range,
      Post => Parents_In_Range (S),
      Exceptional_Cases => (Invalid_Input => True)
   is
      RA, RB : Positive;
   begin
      Find (S, A, RA);
      Find (S, B, RB);
      if RA /= RB then
         if S.Size (RA) < S.Size (RB) then
            S.Parent (RA) := RB;
            Check (S.Size (RB) <= Positive'Last - S.Size (RA),
               "Overflow computing size.");
            S.Size (RB) := S.Size (RB) + S.Size (RA);
         else
            S.Parent (RB) := RA;
            Check (S.Size (RB) <= Positive'Last - S.Size (RA),
               "Overflow computing size.");
            S.Size (RA) := S.Size (RA) + S.Size (RB);
         end if;
         Check (2 <= S.Num_Sets, "Underflow computing number of sets");
         S.Num_Sets := S.Num_Sets - 1;
      end if;
   end Union;

   procedure Read_Input (
      Filename : String;
      Success : out Boolean;
      Points  : out Point_List
   ) is
      File  : File_Type;
      P     : Point;
      Sep   : Character;
   begin
      Success := False; Points := Empty_Vector;
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         while not End_Of_File (File) loop
            Get (File, P.X);
            Get (File, Sep);
            Check (Sep = ',', "Found unexpected separator.");
            Get (File, P.Y);
            Get (File, Sep);
            Check (Sep = ',', "Found unexpected separator.");
            Get (File, P.Z);
            Check (Length (Points) < Last_Count,
               "Points length validation failed.");
            Append (Points, P);
         end loop;
         Close (File);
      end if;
      pragma Assert (not Is_Open (File));
      Success := True;
   exception
      when Data_Error =>
         Put_Line ("Error while parsing a number.");
      when End_Error =>
         Put_Line ("Error: Reached end of input unexpectedly.");
      when Invalid_Input => null;
   end Read_Input;

   function Dist2 (P, Q : Point) return Big_Integer is
      DX, DY, DZ : Big_Integer;
   begin
      DX := To_Big_Integer (P.X) - To_Big_Integer (Q.X);
      DY := To_Big_Integer (P.Y) - To_Big_Integer (Q.Y);
      DZ := To_Big_Integer (P.Z) - To_Big_Integer (Q.Z);
      return DX * DX + DY * DY + DZ * DZ;
   end Dist2;

   procedure Build_and_Sort_Edges (
      Points : Point_List;
      Edges  : out Edge_List
   )
   with
      Post => Are_Edges_Valid (Points, Edges),
      Exceptional_Cases => (Invalid_Input => True)
   is
   begin
      Edges := Edges_Pkg.Empty_Vector;
      for I in First_Index (Points) .. Last_Index (Points) - 1 loop
         pragma Loop_Invariant (Are_Edges_Valid (Points, Edges));
         for J in I + 1 .. Last_Index (Points) loop
            pragma Loop_Invariant (Are_Edges_Valid (Points, Edges));
            declare
               P1 : Point renames Element (Points, I);
               P2 : Point renames Element (Points, J);
            begin
               Check (Edges_Pkg.Length (Edges) < Last_Count,
                  "Edge list length validation failed.");
               Edges_Pkg.Append (Edges, (I, J, Dist2 (P1, P2)));
            end;
         end loop;
      end loop;
      Sort (Edges);
   end Build_and_Sort_Edges;

   procedure Solve_Part_1 (
      Points    : Point_List;
      Edges     : Edge_List;
      Num_Pairs : Positive
   )
   with
      Pre => Length (Points) > 0 and then Are_Edges_Valid (Points, Edges),
      Exceptional_Cases => (Invalid_Input => True)
   is
      package Sizes_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
         (Index_Type => Positive, Element_Type => Big_Integer);

      package Sizes_Sorting is new Sizes_Pkg.Generic_Sorting;

      subtype Size_List is Sizes_Pkg.Vector;

      N     : constant Positive := Integer (Length (Points));
      Boxes : Disjoint_Set (N);
      Sizes : Size_List;
   begin
      Check (Integer (Edges_Pkg.Length (Edges)) >= Num_Pairs,
         "Fewer edges than number of pairs.");
      for I in 1 .. Num_Pairs loop
         pragma Loop_Invariant (Parents_In_Range (Boxes));
         declare
            E : Edge renames Edges_Pkg.Element (Edges, I);
         begin
            Union (Boxes, E.A, E.B);
         end;
      end loop;
      for I in 1 .. N loop
         if Boxes.Parent (I) = I then
            Check (Sizes_Pkg.Length (Sizes) < Last_Count,
               "Size list length validation failed.");
            Sizes_Pkg.Append (Sizes, To_Big_Integer (Boxes.Size (I)));
         end if;
      end loop;
      Sizes_Sorting.Sort (Sizes);
      Check (Sizes_Pkg.Length (Sizes) >= 3, "Fewer than three disjoint sets.");
      declare
         Last_Idx renames Sizes_Pkg.Last_Index (Sizes);
         S1, S2, S3 : Big_Integer;
      begin
         S1 := Sizes_Pkg.Element (Sizes, Last_Idx);
         S2 := Sizes_Pkg.Element (Sizes, Last_Idx - 1);
         S3 := Sizes_Pkg.Element (Sizes, Last_Idx - 2);
         Put ("Answer:");
         Put_Line (To_String (S1 * S2 * S3));
      end;
   end Solve_Part_1;

   procedure Solve_Part_2 (
      Points  : Point_List;
      Edges   : Edge_List
   )
   with
      Pre => Length (Points) > 0 and then Are_Edges_Valid (Points, Edges),
      Exceptional_Cases => (Invalid_Input => True)
   is
      N      : constant Positive := Integer (Length (Points));
      Boxes  : Disjoint_Set (N);
      I      : Integer;
      X1, X2 : Big_Integer := 0;
   begin
      I := Edges_Pkg.First_Index (Edges);
      while Boxes.Num_Sets > 1 and then I <= Edges_Pkg.Last_Index (Edges) loop
         pragma Loop_Invariant (Edges_Pkg.First_Index (Edges) <= I);
         pragma Loop_Invariant (Parents_In_Range (Boxes));
         declare
            E : Edge renames Edges_Pkg.Element (Edges, I);
         begin
            Union (Boxes, E.A, E.B);
            if Boxes.Num_Sets = 1 then
               declare
                  P1 : Point renames Element (Points, E.A);
                  P2 : Point renames Element (Points, E.B);
               begin
                  X1 := To_Big_Integer (P1.X);
                  X2 := To_Big_Integer (P2.X);
               end;
            end if;
         end;
         if I < Edges_Pkg.Last_Index (Edges) then
            I := I + 1;
         end if;
      end loop;
      Put ("Answer:");
      Put_Line (To_String (X1 * X2));
   end Solve_Part_2;

   procedure Process_File (
      Num_Pairs : Natural;
      Filename  : String
   ) is
      Success : Boolean;
      Points  : Point_List;
      Edges   : Edge_List;
   begin
      Read_Input (Filename, Success, Points);
      Check (Length (Points) > 0, "The list of points is empty.");
      if Success then
         Build_and_Sort_Edges (Points, Edges);
         if Num_Pairs = 0 then
            Solve_Part_2 (Points, Edges);
         else
            Solve_Part_1 (Points, Edges, Num_Pairs);
         end if;
      end if;
   exception
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (10, "./share/day08/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (1000, "./share/day08/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (0, "./share/day08/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (0, "./share/day08/input.txt");
end Day08;