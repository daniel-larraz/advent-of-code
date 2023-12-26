pragma Ada_2022;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Day02_Sol;      use Day02_Sol;

procedure Day02 is

   procedure Process_File (
      Filename : String;
      Target : Puzzle_Target
   ) is
      File   : File_Type;
      Sum    : Big_Natural := 0;
      Error  : Boolean;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Process_Line (Get_Line (File), Sum, Error, Target);
         exit when Error;
      end loop;
      Close (File);
      Process_Result (Sum, Error, Target);
   end Process_File;

   procedure Process_File_Part_1 (Filename : String) is
   begin
      Process_File (Filename, Target => Sum_of_Ids);
   end Process_File_Part_1;

   procedure Process_File_Part_2 (Filename : String) is
   begin
      Process_File (Filename, Target => Sum_of_Powers);
   end Process_File_Part_2;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File_Part_1 ("./share/day02/example.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File_Part_1 ("./share/day02/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File_Part_2 ("./share/day02/example.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File_Part_2 ("./share/day02/input.txt");
exception
   when Name_Error =>
      Put_Line ("Error: File does not exist");
   when E : others =>
      Put_Line ("Error while processing input file: " & Exception_Message (E));
end Day02;
