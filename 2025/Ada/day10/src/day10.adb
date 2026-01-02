pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Unbounded_Vectors;
with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

procedure Day10 is

   type Bit is mod 2;

   type Bit_Array is array (Natural range <>) of Bit;

   subtype Light_Diagram is Bit_Array;

   package Schematics_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Natural, Element_Type => Light_Diagram);

   use Schematics_Pkg;

   subtype Schematic_List is Schematics_Pkg.Vector;

   type Nat_Array is array (Natural range <>) of Natural;

   type Int_Array is array (Natural range <>) of Integer;

   function All_Schematics_Have_Length (
      Length : Long_Integer; Schematics : Schematic_List)
   return Boolean is
      (for all I in Schematics =>
         Element (Schematics, I)'First = 0 and then
         Element (Schematics, I)'Length = Length)
   with Ghost;

   function Build_Light_Diagram (Input : String) return Light_Diagram
   with
      Pre => Input'Length > 0,
      Post => Build_Light_Diagram'Result'First = 0 and then
         Build_Light_Diagram'Result'Length > 0
   is
      Diagram : Light_Diagram (0 .. Input'Length - 1) := [others => 0];
      J : Integer := 0;
   begin
      for I in Input'Range loop
         pragma Loop_Invariant (J in Diagram'Range);
         Diagram (J) := (if Input (I) = '#' then 1 else 0);
         if J < Diagram'Last then
            J := J + 1;
         end if;
      end loop;
      return Diagram;
   end Build_Light_Diagram;

   procedure Read_Button_Schematics (
      Num_Lights : Natural;
      Line       : String;
      Schematics : out Schematic_List
   )
   with
      Post => All_Schematics_Have_Length (
         Long_Integer (Num_Lights) + 1, Schematics),
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates
   is
      subtype Schematic is Light_Diagram (0 .. Num_Lights);

      procedure Read_Light_Id (
         Line             : String;
         Lights_To_Toggle : in out Schematic
      )
      with
         Exceptional_Cases => (Invalid_Input => True),
         Always_Terminates
      is
         Id : Natural;
      begin
         Read_Natural (Line, Id);
         Check (Id in Lights_To_Toggle'Range, "Ligth ID is out of range.");
         Lights_To_Toggle (Id) := 1;
      end Read_Light_Id;

      --  The following code triggers a bug when running GNATprove
      --  (see https://github.com/AdaCore/spark2014/issues/62):
      --  procedure Read_Light_Ids is new Fold_Delimited_String (
      --     Accumulator_Type => Schematic,
      --     Initial_Accumulator_Value => [others => 0],
      --     Separator => ',',
      --     Process_Substring => Read_Light_Id
      --  );

      --  So we instantiate the generic procedure manually...
      procedure Read_Light_Ids (
         Input            : String;
         Lights_To_Toggle : out Schematic
      )
      with
         Exceptional_Cases => (Invalid_Input => True),
         Always_Terminates
      is
         S, L : Natural;
      begin
         Lights_To_Toggle := [others => 0];
         if Input'Length > 0 then
            S := Input'First; L := Index (Input, ',');
            while S < L and then L < Input'Last loop
               pragma Loop_Invariant (Input'First <= S);
               pragma Loop_Variant (Increases => L);
               Read_Light_Id (Input (S .. L - 1), Lights_To_Toggle);
               S := L + 1;
               L := Index (Input (S .. Input'Last), ',');
            end loop;
            Read_Light_Id (Input (S .. Input'Last), Lights_To_Toggle);
         end if;
      end Read_Light_Ids;

      procedure Read_Schematic (
         Line       : String;
         Schematics : in out Schematic_List
      )
      with
         Pre => All_Schematics_Have_Length (Schematic'Length, Schematics),
         Post => All_Schematics_Have_Length (Schematic'Length, Schematics),
         Exceptional_Cases => (Invalid_Input => True),
         Always_Terminates
      is
         Lights_To_Toggle : Schematic;
      begin
         Check (Line'Length >= 3,
            "Button schematic has fewer than 3 characters.");
         declare
            Schematic_Body   : constant String :=
               Line (Line'First + 1 .. Line'Last - 1);
         begin
            Read_Light_Ids (Schematic_Body, Lights_To_Toggle);
            Check (Length (Schematics) < Last_Count,
               "Schematics length validation failed.");
            Append (Schematics, Lights_To_Toggle);
         end;
      end Read_Schematic;

      procedure Read_Schematics (
         Input      : String;
         Schematics : out Schematic_List
      )
      with
         Post => All_Schematics_Have_Length (Schematic'Length, Schematics),
         Exceptional_Cases => (Invalid_Input => True),
         Always_Terminates
      is
         S, L : Natural;
      begin
         Schematics := Empty_Vector;
         if Input'Length > 0 then
            S := Input'First; L := Index (Input, ' ');
            while S < L and then L < Input'Last loop
               pragma Loop_Invariant (
                  All_Schematics_Have_Length (Schematic'Length, Schematics)
               );
               pragma Loop_Invariant (Input'First <= S);
               pragma Loop_Variant (Increases => L);
               Read_Schematic (Input (S .. L - 1), Schematics);
               S := L + 1;
               L := Index (Input (S .. Input'Last), ' ');
            end loop;
            Read_Schematic (Input (S .. Input'Last), Schematics);
         end if;
      end Read_Schematics;

   begin
      Read_Schematics (Line, Schematics);
   end Read_Button_Schematics;

   procedure Read_Joltage_Requirements (
      Line : String;
      Reqs : out Int_Array
   )
   with
      Post => Reqs (Reqs'Range)'Initialized and then
         (for all I in Reqs'Range => Reqs (I) >= 0),
      Exceptional_Cases => (Invalid_Input => True),
      Always_Terminates,
      Relaxed_Initialization => Reqs
   is
      Idx : Integer := Line'First;
      Val, Last : Integer;
   begin
      Check (Line'Length > 0,
         "List of joltage requirements is empty.");
      for I in Reqs'Range loop
         Get (Line (Idx .. Line'Last), Val, Last);
         Check (Val >= 0, "Joltage requirement cannot be negative.");
         Reqs (I) := Val;
         if I < Reqs'Last then
            Check (Idx <= Last and then Last <= Line'Last - 2,
               "Fewer joltage requirements than expected.");
            Idx := Last + 2;
         end if;
         pragma Loop_Invariant (Idx in Line'Range);
         pragma Loop_Invariant (Reqs (Reqs'First .. I)'Initialized);
         pragma Loop_Invariant (
            for all J in Reqs'First .. I => Reqs (J) >= 0
         );
      end loop;
   exception
      when Data_Error =>
         Put_Line ("Error while parsing joltage value.");
         raise Invalid_Input;
   end Read_Joltage_Requirements;

   function Is_Valid_Combination (N : Positive; Comb : Nat_Array)
      return Boolean is
   (
      Comb'First = 0 and then Comb'Last <= N - 1 and then
      (for all I in Comb'Range => Comb (I) in 0 .. N - 1) and then
      (for all I in 1 .. Comb'Last => Comb (I) > Comb (I - 1))
   )
   with Ghost;

   procedure Compute_Next_Combination (
      N : Positive;
      Comb : in out Nat_Array;
      Success : out Boolean
   )
   with
      Pre => Is_Valid_Combination (N, Comb),
      Post => Comb'Length = Comb'Old'Length and then
         Is_Valid_Combination (N, Comb)
   is
      I : Integer := Comb'Last;
   begin
      Success := False;
      while I >= Comb'First and then
         Comb (I) >= N - Comb'Last + I - 1
      loop
         pragma Loop_Invariant (I <= Comb'Last);
         pragma Loop_Variant (Decreases => I);
         I := I - 1;
      end loop;
      if I >= Comb'First then
         Comb (I) := Comb (I) + 1;
         for J in I + 1 .. Comb'Last loop
            pragma Loop_Invariant (Is_Valid_Combination (N, Comb));
            Comb (J) := Comb (J - 1) + 1;
         end loop;
         Success := True;
      end if;
   end Compute_Next_Combination;

   procedure Compute_Minimum_Number_of_Presses_P1 (
      Diagram     : Light_Diagram;
      Schematics  : Schematic_List;
      Num_Presses : out Big_Integer
   )
   with
      Pre => Diagram'First = 0 and then
         First_Index (Schematics) = 0 and then
         Length (Schematics) > 0 and then
         All_Schematics_Have_Length (
            Long_Integer (Diagram'Last) + 1, Schematics)
   is
      N : constant Positive := Last_Index (Schematics) + 1;

      function Apply (Comb : Nat_Array) return Light_Diagram
      with
         Pre => Is_Valid_Combination (N, Comb) and then
            All_Schematics_Have_Length (
               Long_Integer (Diagram'Last) + 1, Schematics),
         Post => Apply'Result'First = Diagram'First and then
            Apply'Result'Last = Diagram'Last
      is
         R : Light_Diagram (Diagram'Range) := (others => 0);
      begin
         for J in Comb'Range loop
            for I in Diagram'Range loop
               declare
                  S renames Element (Schematics, Comb (J));
               begin
                  R (I) := R (I) xor S (I);
               end;
            end loop;
         end loop;
         return R;
      end Apply;

      function Equal (A, B : Light_Diagram) return Boolean
      with
         Pre => (A'First = B'First and then A'Last = B'Last)
      is
      begin
         for I in A'Range loop
            if A (I) /= B (I) then
               return False;
            end if;
         end loop;
         return True;
      end Equal;

   begin
      Num_Presses := To_Big_Integer (N);
      for K in 0 .. N loop
         declare
            Success : Boolean;
            Comb : Nat_Array (0 .. K - 1) := [for I in 0 .. K - 1 => I];
         begin
            loop
               pragma Loop_Invariant (Is_Valid_Combination (N, Comb));
               if Equal (Apply (Comb), Diagram) then
                  Num_Presses := To_Big_Integer (K);
                  return;
               end if;

               Compute_Next_Combination (N, Comb, Success);

               exit when not Success;
            end loop;
         end;
      end loop;
   end Compute_Minimum_Number_of_Presses_P1;

   function Min (A : Int_Array) return Integer
   with
      Post => (Min'Result < 0 or else
         (for all I in A'Range => A (I) >= 0))
   is
      M : Integer := Integer'Last;
   begin
      for I in A'Range loop
         pragma Loop_Invariant (
            M < 0 or else
            (for all J in A'First .. I - 1 => A (J) >= 0)
         );
         if A (I) < M then
            M := A (I);
         end if;
      end loop;
      return M;
   end Min;

   function Rest (A : Int_Array) return Int_Array is
     (if A'First = Integer'Last then [] else A (A'First + 1 .. A'Last));

   function Sum (A : Int_Array) return Big_Integer is (
      if A'Length = 0 then 0
      else To_Big_Integer (A (A'First)) + Sum (Rest (A))
   ) with
      Subprogram_Variant => (Decreases => A'Length);

   procedure Lemma_Sum_Non_Negative (A : Int_Array) with
      Ghost,
      Pre => (for all I in A'Range => A (I) >= 0),
      Post => Sum (A) >= 0,
      Always_Terminates,
      Subprogram_Variant => (Decreases => A'Length)
   is
   begin
      if A'Length = 0 or else A'First = Integer'Last then
         null;
      else
         Lemma_Sum_Non_Negative (A (A'First + 1 .. A'Last));
      end if;
   end Lemma_Sum_Non_Negative;

   procedure Lemma_Sum_All_Zero (A : Int_Array)
   with
      Ghost,
      Pre  => (for all I in A'Range => A (I) = 0),
      Post => Sum (A) = 0,
      Always_Terminates,
      Subprogram_Variant => (Decreases => A'Length)
   is
   begin
      if A'Length = 0 or else A'First = Integer'Last then
         null;
      else
         Lemma_Sum_All_Zero (A (A'First + 1 .. A'Last));
      end if;
   end Lemma_Sum_All_Zero;

   function Get_Positive_Element (A : Int_Array) return Natural
   with
      Ghost,
      Pre  => (for all I in A'Range => A (I) >= 0) and then
         Sum (A) > 0,
      Post => Get_Positive_Element'Result in A'Range and then
         A (Get_Positive_Element'Result) > 0
   is
   begin
      for I in A'Range loop
         if A (I) > 0 then
            return I;
         end if;
         Lemma_Sum_All_Zero (A (A'First .. I));
         pragma Loop_Invariant (
            for all J in A'First .. I => A (J) = 0
         );
         pragma Loop_Invariant (
            Sum (A (A'First .. I)) = 0
         );
      end loop;
      return 0;
   end Get_Positive_Element;

   procedure Lemma_Sum_A_Less_Than_Or_Equal_To_Sum_B (A, B : Int_Array)
   with
      Ghost,
      Pre => A'First = B'First and then A'Last = B'Last and then
         (for all I in A'Range => 0 <= A (I) and then A (I) <= B (I)),
      Post => Sum (A) <= Sum (B),
      Always_Terminates,
      Subprogram_Variant => (Decreases => A'Length)
   is
   begin
      if A'Length = 0  or else A'First = Integer'Last then
         null;
      else
         Lemma_Sum_A_Less_Than_Or_Equal_To_Sum_B (Rest (A), Rest (B));
      end if;
   end Lemma_Sum_A_Less_Than_Or_Equal_To_Sum_B;

   procedure Lemma_Sum_A_Less_Than_Sum_B (A, B : Int_Array; P : Natural)
   with
      Ghost,
      Pre => A'First = B'First and then A'Last = B'Last and then
         P in A'Range and then A (P) < B (P) and then
         (for all I in A'Range => 0 <= A (I) and then A (I) <= B (I)),
      Post => Sum (A) < Sum (B),
      Always_Terminates,
      Subprogram_Variant => (Decreases => A'Length)
   is
   begin
      if A'First = P then
         Lemma_Sum_A_Less_Than_Or_Equal_To_Sum_B (Rest (A), Rest (B));
      else
         Lemma_Sum_A_Less_Than_Sum_B (Rest (A), Rest (B), P);
      end if;
   end Lemma_Sum_A_Less_Than_Sum_B;

   procedure Compute_Minimum_Number_of_Presses_P2 (
      Schematics  : Schematic_List;
      Reqs        : Int_Array;
      Num_Presses : out Big_Integer
   )
   --  Implementation of this solution: https://redd.it/1pk87hl
   with
      Pre => Reqs'First = 0 and then
         (for all I in Reqs'Range => Reqs (I) >= 0) and then
         First_Index (Schematics) = 0 and then
         Length (Schematics) > 0 and then
         All_Schematics_Have_Length (
            Long_Integer (Reqs'Last) + 1, Schematics),
      Exceptional_Cases => (Invalid_Input => True)
   is
      subtype Joltage_Array is Int_Array (Reqs'Range);

      subtype Supply_Array is Nat_Array (Reqs'Range);

      subtype Parity_Array is Bit_Array (Reqs'Range);

      type Comb_Info is record
         Num_Buttons : Natural;
         Supply      : Supply_Array;
      end record;

      package Comb_Info_Vectors is new
         SPARK.Containers.Formal.Unbounded_Vectors (
            Index_Type => Natural, Element_Type => Comb_Info);

      use Comb_Info_Vectors;

      subtype Comb_Info_Vec is Comb_Info_Vectors.Vector;

      package Comb_Info_Maps is new
         SPARK.Containers.Formal.Unbounded_Ordered_Maps (
            Key_Type => Bit_Array, Element_Type => Comb_Info_Vec);

      use Comb_Info_Maps;

      subtype Comb_Info_Map is Comb_Info_Maps.Map;

      N : constant Positive := Last_Index (Schematics) + 1;
      Info_Map : Comb_Info_Map;

      procedure Compute_Supply_and_Parity (
         Schematics : Schematic_List;
         Comb       : Nat_Array;
         Supply     : out Supply_Array;
         Parity     : out Parity_Array
      )
      with
         Pre => First_Index (Schematics) = 0 and then
            Last_Index (Schematics) = N - 1 and then
            Is_Valid_Combination (N, Comb) and then
            All_Schematics_Have_Length (
               Long_Integer (Reqs'Last) + 1, Schematics)
      is
      begin
         for I in Supply'Range loop
            declare
               Count : Natural := 0;
            begin
               for J in Comb'Range loop
                  pragma Assert (Comb (J) in 0 .. N - 1);
                  pragma Loop_Invariant (Count <= J - Comb'First);
                  declare
                     S : constant Bit := Element (Schematics, Comb (J)) (I);
                  begin
                     pragma Assert (Natural (S) in 0 .. 1);
                     Count := Count + Natural (S);
                  end;
               end loop;
               Supply (I) := Count;
               Parity (I) := Bit (Count mod 2);
            end;
         end loop;
      end Compute_Supply_and_Parity;

      procedure Add_To_Map (
         Info_Map : in out Comb_Info_Map;
         Parity   : Parity_Array;
         Info     : Comb_Info
      )
      with
         Exceptional_Cases => (Invalid_Input => True)
      is
         Pos : Comb_Info_Maps.Cursor;
      begin
         Pos := Find (Info_Map, Parity);
         if Pos /= Comb_Info_Maps.No_Element then
            declare
               L : Comb_Info_Vec := Element (Info_Map, Pos);
            begin
               Check (Length (L) < Comb_Info_Vectors.Last_Count,
                  "Vector length validation failed."
               );
               Append (L, Info);
               Replace_Element (Info_Map, Pos, L);
            end;
         else
            declare
               L : Comb_Info_Vec;
            begin
               Comb_Info_Vectors.Append (L, Info);
               Check (Length (Info_Map) < Count_Type'Last,
                  "Map length validation failed."
               );
               Insert (Info_Map, Parity, L);
            end;
         end if;
      end Add_To_Map;

      function Compute (Counters : Joltage_Array) return Big_Integer
      with
         Pre => (for all I in Counters'Range => Counters (I) >= 0) and then
            Sum (Counters) >= 0,
         Subprogram_Variant => (Decreases => Sum (Counters))
      is
         Answer : Big_Integer := To_Big_Integer (Integer'Last);
      begin
         if Sum (Counters) = 0 then
            return 0;
         end if;
         pragma Assert (Sum (Counters) > 0);
         declare
            Parity : constant Parity_Array :=
               [for I in Counters'Range => Bit (Counters (I) mod 2)];
            Pos : Comb_Info_Maps.Cursor;
         begin
            Pos := Find (Info_Map, Parity);
            if Pos = Comb_Info_Maps.No_Element then
               return Answer;
            end if;
            declare
               L : constant Comb_Info_Vec := Element (Info_Map, Pos);
            begin
               for I in First_Index (L) .. Last_Index (L) loop
                  declare
                     My_Info : constant Comb_Info := Element (L, I);
                     Remain  : Joltage_Array with Relaxed_Initialization;
                     Res     : Big_Integer;
                     Pos     : constant Natural :=
                        Get_Positive_Element (Counters) with Ghost;
                  begin
                     for J in Remain'Range loop
                        Remain (J) := (Counters (J) - My_Info.Supply (J)) / 2;
                        pragma Loop_Invariant (
                           Remain (Remain'First .. J)'Initialized
                        );
                        pragma Loop_Invariant (
                           for all K in Remain'First .. J =>
                              Remain (K) =
                                 (Counters (K) - My_Info.Supply (K)) / 2
                        );
                     end loop;
                     pragma Assert (
                        for all K in Remain'Range =>
                           Remain (K) = (Counters (K) - My_Info.Supply (K)) / 2
                     );
                     pragma Assert (
                        for all K in Remain'Range => Remain (K) <= Counters (K)
                     );
                     pragma Assert (
                        Remain (Pos) < Counters (Pos)
                     );
                     if Min (Remain) < 0 then
                        Res := Answer;
                     else
                        pragma Assert (
                           for all I in Remain'Range => Remain (I) >= 0
                        );
                        Lemma_Sum_A_Less_Than_Sum_B (Remain, Counters, Pos);
                        Lemma_Sum_Non_Negative (Remain);
                        Res := Compute (Remain);
                     end if;
                     Answer := Min (Answer,
                        To_Big_Integer (My_Info.Num_Buttons) + 2 * Res);
                  end;
               end loop;
            end;
         end;
         return Answer;
      end Compute;

   begin
      Num_Presses := To_Big_Integer (N);
      for K in 0 .. N loop
         declare
            Success : Boolean;
            Comb : Nat_Array (0 .. K - 1) := [for I in 0 .. K - 1 => I];
         begin
            loop
               pragma Loop_Invariant (Is_Valid_Combination (N, Comb));
               declare
                  Supply : Supply_Array;
                  Parity : Parity_Array;
               begin
                  Compute_Supply_and_Parity (Schematics, Comb, Supply, Parity);

                  Add_To_Map (Info_Map, Parity, (Comb'Length, Supply));

                  Compute_Next_Combination (N, Comb, Success);

                  exit when not Success;
               end;
            end loop;
         end;
      end loop;
      Lemma_Sum_Non_Negative (Reqs);
      Num_Presses := Compute (Reqs);
   end Compute_Minimum_Number_of_Presses_P2;

   procedure Process_Line (
      Line        : String;
      Part        : Puzzle_Part;
      Num_Presses : in out Big_Integer
   ) with
      Exceptional_Cases => (Invalid_Input => True)
   is
      Idx         : Natural;
      Schematics  : Schematic_List;
   begin
      Check (Line'Length >= 11,
         "Line has fewer characters than the minimum expected.");
      Idx := Index (Line, ' ');
      Check (Idx >= Line'First + 3,
         "Indicator light diagram has fewer than 3 characters.");
      Check (Line'Last - Idx >= 7,
         "Line has fewer characters than the minimum expected.");
      declare
         Start   : constant Natural := Idx + 1;
         Diagram : constant Light_Diagram :=
            Build_Light_Diagram (Line (Line'First + 1 .. Idx - 2));
      begin
         Idx := Index (Line (Start .. Line'Last), '{');
         Check (Idx > Start, "Separator character '{' not found.");
         Check (Line'Last - Idx >= 2,
            "Line has fewer characters than the minimum expected.");
         Read_Button_Schematics (Diagram'Last,
            Line (Start .. Idx - 2), Schematics);
         Check (Length (Schematics) > 0,
            "The list of button schematics is empty.");
         declare
            Min_Presses : Big_Integer;
         begin
            if Part = Part_1 then
               Compute_Minimum_Number_of_Presses_P1 (
                  Diagram, Schematics, Min_Presses);
            else
               declare
                  Reqs    : Int_Array (0 .. Diagram'Last);
               begin
                  Read_Joltage_Requirements (
                     Line (Idx + 1 .. Line'Last), Reqs);
                  Compute_Minimum_Number_of_Presses_P2 (
                     Schematics, Reqs, Min_Presses);
               end;
            end if;
            Num_Presses := Num_Presses + Min_Presses;
         end;
      end;
   end Process_Line;

   function Invariant (UNUSED : Big_Integer) return Boolean is (True);

   procedure Process_Input is new Process_File_Line_by_Line (
      Data_Type => Big_Integer,
      Invariant => Invariant,
      Process_Line => Process_Line
   );

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      Success     : Boolean;
      Num_Presses : Big_Integer := 0;
   begin
      Process_Input (Filename, Part, Success, Num_Presses);
      if Success then
         Put ("Number of button presses:");
         Put_Line (To_String (Num_Presses));
      end if;
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day10/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day10/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day10/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day10/input.txt");
end Day10;
