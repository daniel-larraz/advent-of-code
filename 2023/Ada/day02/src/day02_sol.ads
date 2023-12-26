pragma Spark_Mode (On);
pragma Ada_2022;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day02_Sol is

   type Puzzle_Target is (Sum_of_Ids, Sum_of_Powers);

   procedure Process_Line (
      Line   : String;
      Sum    : in out Big_Natural;
      Error  : out Boolean;
      Target : Puzzle_Target);

   procedure Process_Result (
      Sum    : Big_Natural;
      Error  : Boolean;
      Target : Puzzle_Target);

end Day02_Sol;
