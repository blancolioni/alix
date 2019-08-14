with Ada.Text_IO;

with Alix.Installer;
with Alix.Versions;

package body Alix.Commands.Install is

   type Install_Command is
     new Root_Alix_Command with null record;

   overriding procedure Execute
     (Command   : Install_Command;
      Arguments : Argument_Vectors.Vector);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Install_Command;
      Arguments : Argument_Vectors.Vector)
   is
      pragma Unreferenced (Command);
   begin
      if Arguments.Last_Index /= 1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Usage: install project-name");
         return;
      end if;

      declare
         Project_And_Version : constant String :=
           Arguments.First_Element;
         Project_Name        : constant String :=
           Alix.Versions.Get_Project_Name
             (Project_And_Version);
         Project_Version     : constant Alix.Versions.Version_Number :=
           Alix.Versions.Get_Project_Version
             (Project_And_Version);
      begin
         Alix.Installer.Install (Project_Name, Project_Version);
      end;

   end Execute;

   ---------------------
   -- Install_Handler --
   ---------------------

   function Handler return Root_Alix_Command'Class is
   begin
      return Install : Install_Command;
   end Handler;

end Alix.Commands.Install;
