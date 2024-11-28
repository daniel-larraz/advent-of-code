pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Day01_Pkg; use Day01_Pkg;

procedure Day01 is

   procedure Open_Input_File (
      File : in out File_Type;
      Name : String
   ) with
      Pre  => not Is_Open (File),
      Post => not Is_Open (File) or else Mode (File) = In_File;

   procedure Open_Input_File (
      File : in out File_Type;
      Name : String
   ) is
   begin
      Open (File, In_File, Name);
   exception
      when Name_Error =>
         Put ("Error: File '"); Put (Name); Put_Line ("' does not exist.");
      when Use_Error =>
         Put ("Error: Couldn't open file '"); Put (Name); Put_Line ("'.");
   end Open_Input_File;

   procedure Process_File_and_Close (
      File          : in out File_Type;
      Only_Numerals : Boolean
   ) with
      Pre  => Is_Open (File) and then Mode (File) = In_File,
      Post => not Is_Open (File);

   procedure Process_File_and_Close (
      File          : in out File_Type;
      Only_Numerals : Boolean
   ) is
      Success : Boolean := False;
      Sum     : Natural := 0;
      Line    : String (1 .. 1024) with Relaxed_Initialization;
      Last    : Natural;
   begin
      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Line'First <= Last then
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
      Close (File);
      Process_Result (Success, Sum);
   exception
      when End_Error => Close (File);
   end Process_File_and_Close;

   procedure Process_File (
      Filename      : String;
      Only_Numerals : Boolean
   ) is
      File : File_Type;
   begin
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         Process_File_and_Close (File, Only_Numerals);
      end if;
      pragma Assert (not Is_Open (File));
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
end Day01;
