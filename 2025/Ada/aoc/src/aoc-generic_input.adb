pragma Spark_Mode (On);

with Ada.Containers; use Ada.Containers;

package body AOC.Generic_Input is

   use Generic_Input_Pkg;

   procedure Read_Generic_Input (
      Filename : String;
      Success  : out Boolean;
      Input    : out Generic_Input_Type
   )
   is
      procedure Read_Line (
         Line   : String;
         UNUSED : Puzzle_Part;
         Input  : in out Generic_Input_Type
      ) with
         Exceptional_Cases => (Invalid_Input => True),
         Always_Terminates
      is
      begin
         Check (Length (Input) < Last_Count, "Input length validation failed");
         Append (Input, Line);
      end Read_Line;

      function Invariant (UNUSED : Generic_Input_Type)
         return Boolean is (True);

      procedure Read_Input is new Process_File_Line_by_Line (
         Max_Line_Length => Max_Line_Length,
         Data_Type => Generic_Input_Type,
         Invariant => Invariant,
         Process_Line => Read_Line
      );

   begin
      Input := Generic_Input_Pkg.Empty_Vector;
      Read_Input (Filename, Part_1, Success, Input);
   end Read_Generic_Input;

   function Create_Grid_From_Input
      (Input : Generic_Input_Type) return Grid_Type
   is
      procedure Compute_Dimensions (
         Input    : Generic_Input_Type;
         Success  : out Boolean;
         Num_Rows : out Natural;
         Num_Cols : out Natural
      )
      with
         Global => null,
         Post => not Success or else
            (Num_Rows > 0 and then Num_Cols > 0 and then
            Num_Rows = Natural (Length (Input)) and then
            (for all I in Input =>
               Str_Length (Element (Input, I)) = Num_Cols)),
         Always_Terminates
      is
      begin
         Success := False; Num_Cols := 0;
         Num_Rows := Natural (Length (Input));
         if Num_Rows = 0 then
            return;
         end if;
         Num_Cols := Str_Length (Element (Input, First_Index (Input)));
         if Num_Cols = 0 then
            return;
         end if;
         for I in First_Index (Input) + 1 .. Last_Index (Input) loop
            pragma Loop_Invariant (for all J in First_Index (Input) .. I - 1 =>
               Str_Length (Element (Input, J)) = Num_Cols);
            if Str_Length (Element (Input, I)) /= Num_Cols then
               return;
            end if;
         end loop;
         Success := True;
      end Compute_Dimensions;

      function Create_Grid (
         Input : Generic_Input_Type; Num_Rows, Num_Cols : Natural
      ) return Grid_Type
      with
         Pre => Num_Rows > 0 and then Num_Cols > 0 and then
            Num_Rows = Natural (Length (Input)) and then
            (for all I in Input =>
               Str_Length (Element (Input, I)) = Num_Cols)
      is
      begin
         declare
            Grid : Grid_Type := [1 .. Num_Rows => [1 .. Num_Cols => '?']];
         begin
            for I in Input loop
               declare
                  Line : String renames Element (Input, I);
               begin
                  for J in Line'Range loop
                     Grid (I, J - Line'First + 1) := Line (J);
                  end loop;
               end;
            end loop;
            return Grid;
         end;
      end Create_Grid;

      Empty_Grid : constant Grid_Type := [2 .. 1 => [2 .. 1 => ' ']];
      Success    : Boolean;
      Num_Rows   : Natural;
      Num_Cols   : Natural;
   begin
      Compute_Dimensions (Input, Success, Num_Rows, Num_Cols);
      if Success then
         return Create_Grid (Input, Num_Rows, Num_Cols);
      else
         return Empty_Grid;
      end if;
   end Create_Grid_From_Input;

end AOC.Generic_Input;