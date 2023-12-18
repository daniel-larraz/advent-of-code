pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;

package body Day01_Pkg is

   procedure Process_Line (
      Line          : String;
      Success       : out Boolean;
      Sum           : in out Natural;
      Only_Numerals : Boolean := True) is

      One   : constant String := "one";
      Two   : constant String := "two";
      Three : constant String := "three";
      Four  : constant String := "four";
      Five  : constant String := "five";
      Six   : constant String := "six";
      Seven : constant String := "seven";
      Eight : constant String := "eight";
      Nine  : constant String := "nine";

      function To_Digit (C : Character) return Digit is
      begin
         return Character'Pos (C) - Character'Pos ('0');
      end To_Digit;

      function Is_Digit_In_Letters (
         Line : String;
         I : Natural;
         D : String) return Boolean
      with
         Pre => I in Line'Range
      is
      begin
         return Line'Last - I >= D'Length - 1 and then
                Line (I .. (I + (D'Length - 1))) = D;
      end Is_Digit_In_Letters;

      procedure Get_Digit (
         Line : String;
         I : Natural;
         Success : out Boolean;
         D : out Digit) is
      begin
         if Line (I) in '0' .. '9' then
            Success := True; D := To_Digit (Line (I));
         elsif Only_Numerals then
            Success := False; D := 0;
         else
            if Is_Digit_In_Letters (Line, I, One) then
               Success := True; D := 1;
            elsif Is_Digit_In_Letters (Line, I, Two) then
               Success := True; D := 2;
            elsif Is_Digit_In_Letters (Line, I, Three) then
               Success := True; D := 3;
            elsif Is_Digit_In_Letters (Line, I, Four) then
               Success := True; D := 4;
            elsif Is_Digit_In_Letters (Line, I, Five) then
               Success := True; D := 5;
            elsif Is_Digit_In_Letters (Line, I, Six) then
               Success := True; D := 6;
            elsif Is_Digit_In_Letters (Line, I, Seven) then
               Success := True; D := 7;
            elsif Is_Digit_In_Letters (Line, I, Eight) then
               Success := True; D := 8;
            elsif Is_Digit_In_Letters (Line, I, Nine) then
               Success := True; D := 9;
            else
               Success := False; D := 0;
            end if;
         end if;
      end Get_Digit;

      procedure Get_First_Digit (
         Line : String;
         Success : out Boolean;
         D : out Digit) is
      begin
         Success := False; D := 0;
         for I in Line'Range loop
            Get_Digit (Line, I, Success, D);
            exit when Success;
         end loop;
      end Get_First_Digit;

      procedure Get_Last_Digit (
         Line : String;
         Success : out Boolean;
         D : out Digit)
      with
         Post => 0 <= D and then D <= 9
      is
      begin
         Success := False; D := 0;
         for I in reverse Line'Range loop
            Get_Digit (Line, I, Success, D);
            exit when Success;
         end loop;
      end Get_Last_Digit;

      procedure Get_Calibration_Value (
         Line    : String;
         Success : out Boolean;
         Value   : out Calibration_Value) is

         D : Digit;
      begin
         Get_First_Digit (Line, Success, D);
         if Success then
            Value := Calibration_Value (10 * D);
            Get_Last_Digit (Line, Success, D);
            Value := Value + Calibration_Value (D);
         else
            Value := 0;
         end if;
      end Get_Calibration_Value;

      Value : Calibration_Value;
   begin
      Get_Calibration_Value (Line, Success, Value);
      Success := Success and then (Natural'Last - Sum) >= Natural (Value);
      if Success then
         Sum := Sum + Natural (Value);
      end if;
   end Process_Line;

   procedure Process_Result (
      Success : Boolean;
      Sum     : Natural) is
   begin
      if Success then
         Put_Line ("Sum of calibration values:" & Sum'Image);
      else
         Put_Line ("Error: Invalid input");
      end if;
   end Process_Result;

end Day01_Pkg;
