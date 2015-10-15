with Ada.Text_IO;

with Alix.Installer;
with Alix.Versions;

package body Alix.Commands.Install is

   type Install_Handler_Type is
     new WL.Command_Line.Dispatch_Handler with null record;

   overriding procedure Execute (Handler : Install_Handler_Type);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Handler : Install_Handler_Type) is
      pragma Unreferenced (Handler);
   begin
      if WL.Command_Line.Argument_Count /= 1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Usage: install project-name");
         return;
      end if;

      declare
         Project_And_Version : constant String :=
                                 WL.Command_Line.Argument (1);
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

   function Install_Handler
      return WL.Command_Line.Root_Argument_Handler'Class
   is
   begin
      return Install : Install_Handler_Type do
         null;
      end return;
   end Install_Handler;

end Alix.Commands.Install;
