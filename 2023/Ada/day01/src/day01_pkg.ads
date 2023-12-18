pragma Spark_Mode (On);

package Day01_Pkg is

   type Digit is range 0 .. 9;

   type Calibration_Value is range 0 .. 99;

   procedure Process_Line (
      Line          : String;
      Success       : out Boolean;
      Sum           : in out Natural;
      Only_Numerals : Boolean := True);

   procedure Process_Result (
      Success : Boolean;
      Sum     : Natural);

end Day01_Pkg;
