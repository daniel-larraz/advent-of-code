pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;

procedure Day09 is

   type Point is record
      X, Y : Big_Integer;
   end record;

   package Points_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => Point);

   use Points_Pkg;

   subtype Point_List is Points_Pkg.Vector;

   procedure Read_Input (
      Filename : String;
      Success : out Boolean;
      Points  : out Point_List
   ) is
      File  : File_Type;
      P     : Point;
      X, Y  : Integer;
      Sep   : Character;
   begin
      Success := False; Points := Empty_Vector;
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         while not End_Of_File (File) loop
            Get (File, X);
            Get (File, Sep);
            Check (Sep = ',', "Found unexpected separator.");
            Get (File, Y);
            P := (To_Big_Integer (X), To_Big_Integer (Y));
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

   function Compute_Area (P1, P2 : Point) return Big_Integer is
      Width  : constant Big_Integer := abs (P1.X - P2.X) + 1;
      Height : constant Big_Integer := abs (P1.Y - P2.Y) + 1;
   begin
      return Width * Height;
   end Compute_Area;

   function On_Edge (P : Point; A, B : Point) return Boolean is
   begin
      if A.X = B.X then
         --  Vertical edge
         return P.X = A.X and then
            P.Y >= Min (A.Y, B.Y) and then P.Y <= Max (A.Y, B.Y);
      elsif A.Y = B.Y then
         --  Horizontal edge
         return P.Y = A.Y and then
            P.X >= Min (A.X, B.X) and then P.X <= Max (A.X, B.X);
      else
         return False;
      end if;
   end On_Edge;

   function Point_Inside_Polygon (P : Point; Poly : Point_List)
      return Boolean
   is
      Inside : Boolean := False;
      N      : constant Integer :=  Integer (Length (Poly));
   begin
      --  First, check if point is on any edge
      for I in 1 .. N loop
         declare
            A : constant Point := Element (Poly, I);
            B : constant Point := Element (Poly, ((I mod N) + 1));
         begin
            if On_Edge (P, A, B) then
               return True;
            end if;
         end;
      end loop;

      --  Ray-casting: horizontal ray to the right
      for I in 1 .. N loop
         declare
            A : constant Point := Element (Poly, I);
            B : constant Point := Element (Poly, ((I mod N) + 1));
         begin
            if (A.Y > P.Y) /= (B.Y > P.Y) then
               if P.X < A.X + (B.X - A.X) * (P.Y - A.Y) / (B.Y - A.Y) then
                  Inside := not Inside;
               end if;
            end if;
         end;
      end loop;
      return Inside;
   end Point_Inside_Polygon;

   function Same_Segment (A, B, C, D : Point) return Boolean is
   begin
      return (A = C and then B = D) or else (A = D and then B = C);
   end Same_Segment;

   function Segments_Intersect (A, B, C, D : Point) return Boolean is
      function CCW (P1, P2, P3 : Point) return Big_Integer is
         ((P2.X - P1.X) * (P3.Y - P1.Y) - (P2.Y - P1.Y) * (P3.X - P1.X));
   begin
      return (CCW (A, C, D) * CCW (B, C, D) < 0) and then
             (CCW (A, B, C) * CCW (A, B, D) < 0);
   end Segments_Intersect;

   type Rectangle_Array is array (1 .. 4) of Point;

   function Rect_Inside_Polygon (P1, P2 : Point; Poly : Point_List)
      return Boolean
   is
      N_Poly : constant Integer := Integer (Length (Poly));
      X_Min  : constant Big_Integer := Min (P1.X, P2.X);
      X_Max  : constant Big_Integer := Max (P1.X, P2.X);
      Y_Min  : constant Big_Integer := Min (P1.Y, P2.Y);
      Y_Max  : constant Big_Integer := Max (P1.Y, P2.Y);
      Rect   : constant Rectangle_Array := [
         (X_Min, Y_Min), (X_Max, Y_Min),
         (X_Max, Y_Max), (X_Min, Y_Max)
      ];
   begin
      --  1) Check all rectangle corners
      for I in Rect'Range loop
         if not Point_Inside_Polygon (Rect (I), Poly) then
            return False;
         end if;
      end loop;

      --  2) Edge intersection checks
      for I in Rect'Range loop
         declare
            A : constant Point := Rect (I);
            B : constant Point := Rect ((I mod Rect'Length) + 1);
         begin
            for J in First_Index (Poly) .. Last_Index (Poly) loop
               declare
                  C : constant Point := Element (Poly, J);
                  D : constant Point := Element (Poly, ((J mod N_Poly) + 1));
               begin
                  if not Same_Segment (A, B, C, D) and then
                     Segments_Intersect (A, B, C, D)
                  then
                     return False;
                  end if;
               end;
            end loop;
         end;
      end loop;

      return True;
   end Rect_Inside_Polygon;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success  : Boolean;
      Points   : Point_List;
      Max_Area : Big_Integer := 0;
   begin
      Read_Input (Filename, Success, Points);
      Check (Length (Points) > 0, "The list of points is empty.");
      if Success then
         for I in First_Index (Points) .. Last_Index (Points) - 1 loop
            for J in I + 1 .. Last_Index (Points) loop
               declare
                  P1 renames Element (Points, I);
                  P2 renames Element (Points, J);
                  Area : constant Big_Integer := Compute_Area (P1, P2);
               begin
                  if Area > Max_Area then
                     if Part = Part_1 or else
                        Rect_Inside_Polygon (P1, P2, Points)
                     then
                        Max_Area := Area;
                     end if;
                  end if;
               end;
            end loop;
         end loop;
         Put ("Largest area:"); Put_Line (To_String (Max_Area));
      end if;
   exception
      when Invalid_Input => null;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day09/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day09/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day09/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day09/input.txt");
end Day09;
