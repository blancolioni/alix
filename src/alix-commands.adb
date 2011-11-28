with WL.Command_Line;

with Alix.Commands.Help;

with Alix.Commands.Configure;
with Alix.Commands.Install;
with Alix.Commands.Update;

package body Alix.Commands is

   type Command_Flag is
     (Skip_Source_Clone);

   Flag_Values : array (Command_Flag) of Boolean := (others => False);

   type Flag_Handler is
     new WL.Command_Line.Flag_Argument_Handler with
      record
         Flag : Command_Flag;
      end record;

   procedure Handle_Clear (Handler : in out Flag_Handler);
   procedure Handle_Set (Handler : in out Flag_Handler);

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      WL.Command_Line.Process_Command_Line;
   end Execute;

   ------------------
   -- Handle_Clear --
   ------------------

   procedure Handle_Clear (Handler : in out Flag_Handler) is
   begin
      Flag_Values (Handler.Flag) := False;
   end Handle_Clear;

   ----------------
   -- Handle_Set --
   ----------------

   procedure Handle_Set (Handler : in out Flag_Handler) is
   begin
      Flag_Values (Handler.Flag) := True;
   end Handle_Set;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      WL.Command_Line.Register
        ("help", '?', Alix.Commands.Help.Help_Handler);

      WL.Command_Line.Register
        ("install", Alix.Commands.Install.Install_Handler);
      WL.Command_Line.Register
        ("configure", Alix.Commands.Configure.Configure_Handler);
      WL.Command_Line.Register
        ("update", Alix.Commands.Update.Update_Handler);

      WL.Command_Line.Register
        ("no-clone",
         Flag_Handler'(WL.Command_Line.Flag_Argument_Handler
           with Flag => Skip_Source_Clone));

   end Initialise;

   -----------------------
   -- Skip_Source_Clone --
   -----------------------

   function Skip_Source_Clone return Boolean is
   begin
      return Flag_Values (Skip_Source_Clone);
   end Skip_Source_Clone;

end Alix.Commands;
