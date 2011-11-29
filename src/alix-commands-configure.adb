with Ada.Directories;
with Ada.Text_IO;

with Alix.Installer;

package body Alix.Commands.Configure is

   type Configure_Handler_Type is
     new WL.Command_Line.Dispatch_Handler with null record;

   procedure Execute (Handler : Configure_Handler_Type);

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
      return WL.Command_Line.Root_Argument_Handler'Class
   is
   begin
      return Configure : Configure_Handler_Type do
        null;
      end return;
   end Configure_Handler;

   -------------
   -- Execute --
   -------------

   procedure Execute (Handler : Configure_Handler_Type) is
      pragma Unreferenced (Handler);
   begin
      if WL.Command_Line.Argument_Count /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Usage: configure");
         return;
      end if;

      Alix.Installer.Configure (Ada.Directories.Current_Directory);

   end Execute;

end Alix.Commands.Configure;
