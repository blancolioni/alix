with Ada.Text_IO;

package body Alix.Commands.Help is

   type Help_Command is new Root_Alix_Command with null record;

   overriding procedure Execute
     (Command   : Help_Command;
      Arguments : Argument_Vectors.Vector);

     -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Help_Command;
      Arguments : Argument_Vectors.Vector)
   is
      pragma Unreferenced (Command);
      pragma Unreferenced (Arguments);
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error,
                "Usage: alix configure");
      Put_Line (Standard_Error,
                "       alix install <project name>");
   end Execute;

   ------------------
   -- Help_Handler --
   ------------------

   function Handler return Root_Alix_Command'Class is
   begin
      return Help : Help_Command;
   end Handler;

end Alix.Commands.Help;
