with Ada.Directories;
with Ada.Text_IO;

with Alix.Installer;

package body Alix.Commands.Configure is

   type Configure_Command is
     new Root_Alix_Command with null record;

   overriding procedure Execute
     (Command   : Configure_Command;
      Arguments : Argument_Vectors.Vector);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Configure_Command;
      Arguments : Argument_Vectors.Vector)
   is
      pragma Unreferenced (Command);
   begin
      if not Arguments.Is_Empty then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Usage: configure");
         return;
      end if;

      Alix.Installer.Configure
        (Directory => Ada.Directories.Current_Directory,
         Mode      => "configure");

   end Execute;

   -------------
   -- Handler --
   -------------

   function Handler return Root_Alix_Command'Class is
   begin
      return Configure : Configure_Command;
   end Handler;

end Alix.Commands.Configure;
