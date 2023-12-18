with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;
with Day01_Pkg; use Day01_Pkg;

procedure Day01 is

   procedure Process_File (
      Filename : String;
      Only_Numerals : Boolean
   ) is
      File    : File_Type;
      Success : Boolean;
      Sum     : Integer := 0;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Process_Line (Get_Line (File), Success, Sum, Only_Numerals);
         exit when not Success;
      end loop;
      Close (File);
      Process_Result (Success, Sum);
   end Process_File;

   procedure Process_File_Part_1 (Filename : String) is
   begin
      Process_File (Filename, Only_Numerals => True);
   end Process_File_Part_1;

   procedure Process_File_Part_2 (Filename : String) is
   begin
      Process_File (Filename, Only_Numerals => False);
   end Process_File_Part_2;

begin
   Put_Line ("# Example - Part 1 #");
   Process_File_Part_1 ("./share/day01/example_part_1.txt");
   Put_Line ("# Input - Part 1 #");
   Process_File_Part_1 ("./share/day01/input.txt");
   Put_Line ("# Example - Part 2 #");
   Process_File_Part_2 ("./share/day01/example_part_2.txt");
   Put_Line ("# Input - Part 2 #");
   Process_File_Part_2 ("./share/day01/input.txt");
exception
   when Name_Error =>
      Put_Line ("Error: File does not exist");
   when E : others =>
      Put_Line ("Error while processing input file: " & Exception_Message (E));
end Day01;
