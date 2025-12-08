pragma Spark_Mode (On);

with SPARK.Containers.Formal.Unbounded_Vectors;

package AOC.Generic_Input is

   package Generic_Input_Pkg is new SPARK.Containers.Formal.Unbounded_Vectors
      (Index_Type => Positive, Element_Type => String);

   subtype Generic_Input_Type is Generic_Input_Pkg.Vector;

   generic
      Max_Line_Length : Positive := 1024;
   procedure Read_Generic_Input (
      Filename : String;
      Success  : out Boolean;
      Input    : out Generic_Input_Type
   );

   type Grid_Type is
      array (Positive range <>, Positive range <>) of Character;

   function Is_Empty (Grid : Grid_Type) return Boolean is
      (Grid'Length (1) = 0 or else Grid'Length (2) = 0);

   function Create_Grid_From_Input
      (Input : Generic_Input_Type) return Grid_Type;

end AOC.Generic_Input;