pragma Spark_Mode (On);

package body AOC is

   procedure Check (Condition : Boolean; Error_Msg: String) is
   begin
      if not Condition then
         if Error_Msg /= "" then
            Put ("Error: "); Put_Line (Error_Msg);
         end if;
         raise Invalid_Input;
      end if;
   end Check;

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

   procedure Process_File_Line_by_Line (
      Filename : String;
      Part     : Puzzle_Part;
      Success  : out Boolean;
      Data     : in out Data_Type
   )
   is
      procedure Process_File (
         File : File_Type
      )
      with
         Pre  => Is_Open (File) and then Mode (File) = In_File,
         Post => Is_Open (File)
      is
         Buffer : String (1 .. Max_Line_Length) with Relaxed_Initialization;
         Last   : Natural;
      begin
         while not End_Of_File (File) loop
            Get_Line (File, Buffer, Last);
            Check (Buffer'First <= Last, "Error while reading line from file");
            declare
               Line : constant String := Buffer (Buffer'First .. Last);
            begin
               Process_Line (Line, Part, Data);
            end;
         end loop;
         Success := True;
      exception
         when End_Error | Invalid_Input => null;
      end Process_File;

      File : File_Type;
   begin
      Success := False;
      Open_Input_File (File, Filename);
      if Is_Open (File) then
         Process_File (File);
         Close (File);
      end if;
      pragma Assert (not Is_Open (File));
   end Process_File_Line_by_Line;

   function Index (Source : String; Sep : Character) return Natural is
   begin
      for I in Source'Range loop
         if Source (I) = Sep then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function Rev_Index (Source : String; Sep : Character) return Natural is
   begin
      for I in reverse Source'Range loop
         if Source (I) = Sep then
            return I;
         end if;
      end loop;
      return 0;
   end Rev_Index;

   function To_Digit (C : Character) return Natural is
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end To_Digit;

   procedure Read_Big_Natural (Input : String; N : out Big_Natural) is
   begin
      N := 0;
      Check (Input'Length > 0, "Empty input in Read_Big_Natural");
      for C of Input loop
         pragma Loop_Invariant (0 <= N);
         Check (C in '0' .. '9', "Unexpected character in Read_Big_Natural");
         N := 10 * N + To_Big_Integer(To_Digit (C));
      end loop;
   end Read_Big_Natural;

end AOC;