pragma Spark_Mode (On);

with AOC; use AOC;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day03 is

   Parse_Error : exception;

   C : Character := '?'; -- Last read character, if any

   procedure Read_Next (
      File    : File_Type;
      Success : out Boolean
   )
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Always_Terminates
   is
   begin
      Get (File, C);
      Success := True;
      exception
         when End_Error => Success := False;
   end Read_Next;

   procedure Parse_Char (
      File     : File_Type;
      Expected : Character
   )
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (Parse_Error | End_Error => True),
      Always_Terminates
   is
   begin
      Get (File, C);
      if C /= Expected then
         raise Parse_Error;
      end if;
   end Parse_Char;

   procedure Parse_Factor (
      File       : File_Type;
      Factor     : out Big_Natural;
      End_Symbol : Character
   )
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (Parse_Error | End_Error => True),
      Always_Terminates
   is
      End_Reached : exception;

      procedure Parse_End_Symbol_Or_Digit (Factor : in out Big_Natural) is
      begin
         Get (File, C);
         if C = End_Symbol then
            raise End_Reached;
         end if;
         if C not in '0' .. '9' then
            raise Parse_Error;
         end if;
         Factor := 10 * Factor + To_Big_Integer (To_Digit (C));
      end Parse_End_Symbol_Or_Digit;

   begin
      Factor := 0;
      Get (File, C);
      if C not in '0' .. '9' then
         raise Parse_Error;
      end if;
      Factor := To_Big_Integer (To_Digit (C));
      Parse_End_Symbol_Or_Digit (Factor);
      Parse_End_Symbol_Or_Digit (Factor);
      Get (File, C);
      if C /= End_Symbol then
         raise Parse_Error;
      end if;
   exception
      when End_Reached => null;
   end Parse_Factor;

   procedure Parse_Mul (
      File    : File_Type;
      X, Y    : out Big_Natural
   )
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (Parse_Error | End_Error => True),
      Always_Terminates
   is
   begin
      X := 0; Y := 0;
      Parse_Char (File, 'u');
      Parse_Char (File, 'l');
      Parse_Char (File, '(');
      Parse_Factor (File, X, ',');
      Parse_Factor (File, Y, ')');
   end Parse_Mul;

   procedure Parse_Do_Or_Dont (
      File    : File_Type;
      Enabled : out Boolean
   )
   with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => Is_Open (File) and then Mode (File) = In_File,
      Exceptional_Cases => (Parse_Error | End_Error => True),
      Always_Terminates
   is
   begin
      Parse_Char (File, 'o');
      Get (File, C);
      if C = '(' then
         Parse_Char (File, ')');
         Enabled := True;
      elsif C = 'n' then
         Parse_Char (File, ''');
         Parse_Char (File, 't');
         Parse_Char (File, '(');
         Parse_Char (File, ')');
         Enabled := False;
      else
         raise Parse_Error;
      end if;
   end Parse_Do_Or_Dont;

   procedure Process_File (Part : Puzzle_Part; Filename : String) is
      File    : File_Type;
      X, Y    : Big_Natural;
      Sum     : Big_Natural := 0;
      Success : Boolean;
      Enabled : Boolean := True;
   begin
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         Read_Next (File, Success);
         while Success loop
            declare
            begin
               if C = 'm' then
                  Parse_Mul (File, X, Y);
                  if Part = Part_1 or else Enabled then
                     Sum := Sum + X * Y;
                  end if;
               elsif Part = Part_2 and then C = 'd' then
                  Parse_Do_Or_Dont (File, Enabled);
               end if;
               Read_Next (File, Success);
            exception
               when Parse_Error => null;
               when End_Error => Success := False;
            end;
         end loop;
         Close (File);
         Put ("Result:"); Put_Line (To_String (Sum));
      end if;
      pragma Assert (not Is_Open (File));
   end Process_File;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File (Part_1, "./share/day03/example_part1.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File (Part_1, "./share/day03/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File (Part_2, "./share/day03/example_part2.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File (Part_2, "./share/day03/input.txt");
   pragma Unreferenced (C);
end Day03;