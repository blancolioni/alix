with Ada.Text_IO;

with Alix.Installer;

package body Alix.Commands.Update is

   type Update_Command is new Root_Alix_Command with null record;

   overriding procedure Execute
     (Command   : Update_Command;
      Arguments : Argument_Vectors.Vector);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Update_Command;
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
         Project_Name : constant String := Arguments.First_Element;
      begin
         Alix.Installer.Install (Project_Name);
      end;
   end Execute;

   -------------
   -- Handler --
   -------------

   function Handler return Root_Alix_Command'Class is
   begin
      return Update : Update_Command;
   end Handler;

end Alix.Commands.Update;
