pragma Spark_Mode (On);
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package AOC is

   Invalid_Input : exception;

   procedure Check (Condition : Boolean; Error_Msg : String) with
   Post => Condition,
   Exceptional_Cases => (Invalid_Input => not Condition);

   procedure Open_Input_File (
      File : in out File_Type;
      Name : String
   ) with
      Pre  => not Is_Open (File),
      Post => not Is_Open (File) or else Mode (File) = In_File;

   type Puzzle_Part is (Part_1, Part_2);

   generic
      type Data_Type is private;
      Max_Line_Length : Positive := 1024;
   with
      procedure Process_Line (
         Line : String;
         Part : Puzzle_Part;
         Data : in out Data_Type);

   procedure Process_File_Line_by_Line (
      Filename : String;
      Part     : Puzzle_Part;
      Success  : out Boolean;
      Data     : in out Data_Type
   );

   function Index (Source : String; Sep : Character) return Natural;

   function Rev_Index (Source : String; Sep : Character) return Natural;

   function To_Digit (C : Character) return Natural
   with
      Pre => C in '0' .. '9',
      Post => To_Digit'Result in 0 .. 9;

   procedure Read_Big_Natural (Input : String; N : out Big_Natural) with
   Exceptional_Cases => (Invalid_Input => True);

end AOC;