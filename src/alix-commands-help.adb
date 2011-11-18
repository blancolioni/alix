package body Alix.Commands.Help is

   type Help_Handler_Type is
     new WL.Command_Line.Flag_Argument_Handler with
     null record;

   procedure Handle_Set (Handler : in out Help_Handler_Type);

   function Exit_After (Handler : Help_Handler_Type) return Boolean;

   ----------------
   -- Exit_After --
   ----------------

   function Exit_After (Handler : Help_Handler_Type) return Boolean is
      pragma Unreferenced (Handler);
   begin
      return True;
   end Exit_After;

   ----------------
   -- Handle_Set --
   ----------------

   procedure Handle_Set (Handler : in out Help_Handler_Type) is
      pragma Unreferenced (Handler);
   begin
      WL.Command_Line.Show_Usage;
   end Handle_Set;

   ------------------
   -- Help_Handler --
   ------------------

   function Help_Handler
     return WL.Command_Line.Root_Argument_Handler'Class
   is
   begin
      return H : Help_Handler_Type do
        null;
      end return;
   end Help_Handler;

end Alix.Commands.Help;

