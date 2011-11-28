with Ada.Text_IO;

with Alix.Updater;

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
      if WL.Command_Line.Argument_Count /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Usage: update");
         return;
      end if;

      Alix.Updater.Update;

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
