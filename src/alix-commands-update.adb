with Ada.Text_IO;

with Alix.Installer;

package body Alix.Commands.Update is

   type Update_Handler_Type is
     new WL.Command_Line.Dispatch_Handler with null record;

   procedure Execute (Handler : Update_Handler_Type);

   -------------
   -- Execute --
   -------------

   procedure Execute (Handler : Update_Handler_Type) is
      pragma Unreferenced (Handler);
   begin
      if WL.Command_Line.Argument_Count /= 1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Usage: install project-name");
         return;
      end if;

      declare
         Project_Name : constant String :=
                          WL.Command_Line.Argument (1);
      begin
         Alix.Installer.Install (Project_Name);
      end;
   end Execute;

   ---------------------
   -- Update_Handler --
   ---------------------

   function Update_Handler
      return WL.Command_Line.Root_Argument_Handler'Class
   is
   begin
      return Update : Update_Handler_Type do
        null;
      end return;
   end Update_Handler;

end Alix.Commands.Update;
