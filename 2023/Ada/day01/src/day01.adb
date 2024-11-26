pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Day01_Pkg; use Day01_Pkg;

procedure Day01 is

   procedure Process_File (
      Filename : String;
      Only_Numerals : Boolean)
   with
      Exceptional_Cases =>
         (Name_Error | Use_Error | End_Error => Standard.True);

   procedure Process_File (
      Filename : String;
      Only_Numerals : Boolean
   ) is
      File    : File_Type;
      Success : Boolean := False;
      Sum     : Natural := 0;
      Line    : String (1 .. 1024) with Relaxed_Initialization;
      Last    : Natural;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Line'First <= Last and then Last <= Line'Last then
            declare
               Input : constant String := Line (Line'First .. Last);
            begin
               Process_Line (Input, Success, Sum, Only_Numerals);
            end;
         else
            Success := False;
         end if;
         exit when not Success;
      end loop;
      pragma Warnings (Off);
      Close (File);
      pragma Warnings (On);
      Process_Result (Success, Sum);
   exception
      when others =>
         if Is_Open (File) then
            pragma Warnings (Off);
            Close (File);
            pragma Warnings (On);
         end if;
         raise;
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
      Put_Line ("Error: File does not exist.");
   when Use_Error =>
      Put_Line ("Error while processing input file.");
   when End_Error =>
      Put_Line ("Unexpected error: end of file reached.");
end Day01;
