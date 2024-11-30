pragma Spark_Mode (On);
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

package body Day02_Sol is

   type Color is (Red, Green, Blue);

   type Game_Set is record
     Red   : Big_Natural;
     Green : Big_Natural;
     Blue  : Big_Natural;
   end record;

   Ref_Set : constant Game_Set := (Red => 12, Green => 13, Blue => 14);

   function Index (Source : String; Sep : Character) return Natural is
   begin
      for I in Source'Range loop
         if Source (I) = Sep then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function To_Digit (C : Character) return Natural
   with
      Pre => C in '0' .. '9',
      Post => To_Digit'Result in 0 .. 9 is
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end To_Digit;

   procedure Process_Line (
      Line   : String;
      Sum    : in out Big_Natural;
      Error  : out Boolean;
      Target : Puzzle_Target) is

      function Is_Set_Possible (Set : Game_Set) return Boolean is
      begin
         return Set.Red <= Ref_Set.Red and then
                Set.Blue <= Ref_Set.Blue and then
                Set.Green <= Ref_Set.Green;
      end Is_Set_Possible;

      procedure Read_Natural (
         Input  : String;
         N      : out Big_Natural;
         Error  : out Boolean) is
      begin
         N := 0; Error := Input'Length = 0;
         if Error then
            return;
         end if;
         for C of Input loop
            pragma Loop_Invariant (0 <= N);
            Error := not (C in '0' .. '9');
            if Error then
               return;
            end if;
            N := 10 * N + To_Big_Integer (To_Digit (C));
         end loop;
         Error := False;
      end Read_Natural;

      procedure Read_Game_Id (
         Input  : String;
         Id    : out Big_Natural;
         Error : out Boolean) is

         Idx : Natural;
      begin
         Id := 0; Idx := Index (Input, ' ');
         Error := not (Input'First < Idx and then Idx < Input'Last);
         if Error then
            return;
         end if;
         Error := not (Input (Input'First .. Idx - 1) = "Game");
         if Error then
            return;
         end if;
         Read_Natural (Input (Idx + 1 .. Input'Last), Id, Error);
      end Read_Game_Id;

      procedure Read_Color (
         Input : String;
         C     : out Color;
         Error : out Boolean) is
      begin
         Error := False; C := Red;
         if Input = "red" then
            C := Red;
         elsif Input = "green" then
            C := Green;
         elsif Input = "blue" then
            C := Blue;
         else
            Error := True;
         end if;
      end Read_Color;

      procedure Read_Set_Item (
         Input : String;
         Set   : in out Game_Set;
         Error : out Boolean) is

         S, Idx : Natural;
         N      : Big_Natural;
         C      : Color;
      begin
         if Input'Length <= 1 or else Input (Input'First) /= ' ' then
            Error := True;
            return;
         end if;
         S := Input'First + 1; -- Skip initial whitespace
         Idx := Index (Input (S .. Input'Last), ' ');
         Error := not (S < Idx and then Idx < Input'Last);
         if Error then
            return;
         end if;
         Read_Natural (Input (S .. Idx - 1), N, Error);
         if Error then
            return;
         end if;
         Read_Color (Input (Idx + 1 .. Input'Last), C, Error);
         if Error then
            return;
         end if;
         case C is
            when Red   => Set.Red   := N;
            when Green => Set.Green := N;
            when Blue  => Set.Blue  := N;
         end case;
      end Read_Set_Item;

      generic
         type Accumulator_Type is private;
         Initial_Accumulator_Value : Accumulator_Type;
         Separator : Character;
      with
         procedure Process_Substring (
            Item  : String;
            Acc   : in out Accumulator_Type;
            Error : out Boolean);
      procedure Fold_Delimited_String (
            Input : String;
            Acc   : out Accumulator_Type;
            Error : out Boolean)
      with Pre  => Input'Length > 0;

      procedure Fold_Delimited_String (
         Input : String;
         Acc   : out Accumulator_Type;
         Error : out Boolean
      ) is
         S : Natural := Input'First;
         L : Natural := Index (Input, Separator);
      begin
         Acc := Initial_Accumulator_Value;
         while S < L and then L < Input'Last loop
            pragma Loop_Invariant (Input'First <= S);
            Process_Substring (Input (S .. L - 1), Acc, Error);
            if Error then
               return;
            end if;
            S := L + 1;
            L := Index (Input (S .. Input'Last), Separator);
         end loop;
         Process_Substring (Input (S .. Input'Last), Acc, Error);
      end Fold_Delimited_String;

      procedure Read_Game_Set is new Fold_Delimited_String (
         Accumulator_Type => Game_Set,
         Initial_Accumulator_Value => (0, 0, 0),
         Separator => ',',
         Process_Substring => Read_Set_Item
      );

      procedure Update_Min_Set_of_Cubes (
         Set : Game_Set;
         Min_Set_Of_Cubes : in out Game_Set) is
      begin
         Min_Set_Of_Cubes := (
            Red   => Max (Min_Set_Of_Cubes.Red, Set.Red),
            Green => Max (Min_Set_Of_Cubes.Green, Set.Green),
            Blue  => Max (Min_Set_Of_Cubes.Blue, Set.Blue)
         );
      end Update_Min_Set_of_Cubes;

      function Power (Set : Game_Set) return Big_Natural is
      begin
         return Set.Red * Set.Green * Set.Blue;
      end Power;

      procedure Process_Game_Set (
         Input            : String;
         Min_Set_Of_Cubes : in out Game_Set;
         Error            : out Boolean
      ) is
         Set : Game_Set;
      begin
         Read_Game_Set (Input, Set, Error);
         if not Error then
            Update_Min_Set_of_Cubes (Set, Min_Set_Of_Cubes);
         end if;
      end Process_Game_Set;

      procedure Process_Game_Sets is new Fold_Delimited_String (
         Accumulator_Type => Game_Set,
         Initial_Accumulator_Value => (0, 0, 0),
         Separator => ';',
         Process_Substring => Process_Game_Set
      );

      procedure Update_Sum (
         Id               : Big_Natural;
         Min_Set_Of_Cubes : Game_Set;
         Sum              : in out Big_Natural
      ) is
      begin
         case Target is
         when Sum_of_Ids =>
            if Is_Set_Possible (Min_Set_Of_Cubes) then
               Sum := Sum + Id;
            end if;
         when Sum_of_Powers =>
            Sum := Sum + Power (Min_Set_Of_Cubes);
         end case;
      end Update_Sum;

      Id  : Big_Natural;
      Idx : Natural;
   begin
      Idx := Index (Line, ':');
      Error := not (Line'First < Idx and then Idx < Line'Last);
      if Error then
         return;
      end if;
      Read_Game_Id (Line (Line'First .. Idx - 1), Id, Error);
      if Error then
         return;
      end if;
      declare
         Min_Set_Of_Cubes : Game_Set;
         Game_Sets_Line : constant String := Line (Idx + 1 .. Line'Last);
      begin
         Process_Game_Sets (Game_Sets_Line, Min_Set_Of_Cubes, Error);
         Update_Sum (Id, Min_Set_Of_Cubes, Sum);
      end;
   end Process_Line;

   procedure Process_Result (
      Sum    : Big_Natural;
      Error  : Boolean;
      Target : Puzzle_Target) is
   begin
      if not Error then
         case Target is
            when Sum_of_Ids =>
               Put ("Sum of the IDs:");
            when Sum_of_Powers =>
               Put ("Sum of the powers:");
         end case;
         Put_Line (To_String (Sum));
      else
         Put_Line ("Error: Invalid input");
      end if;
   end Process_Result;

end Day02_Sol;
