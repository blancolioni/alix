private with Ada.Containers.Indefinite_Vectors;

package Alix.Commands is

   procedure Initialise;

   procedure Execute;

   function Skip_Source_Clone return Boolean;

private

   package Argument_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Root_Alix_Command is abstract tagged null record;

   procedure Execute
     (Command   : Root_Alix_Command;
      Arguments : Argument_Vectors.Vector)
   is abstract;

end Alix.Commands;
