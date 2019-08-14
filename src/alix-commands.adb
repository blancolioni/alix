with Ada.Command_Line;

with WL.String_Maps;

with Alix.Commands.Help;

with Alix.Commands.Configure;
with Alix.Commands.Install;
with Alix.Commands.Update;

package body Alix.Commands is

   package Command_Maps is
     new WL.String_Maps (Root_Alix_Command'Class);

   Command_Map : Command_Maps.Map;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      if Ada.Command_Line.Argument_Count = 0 then
         Alix.Commands.Help.Handler.Execute
           (Argument_Vectors.Empty_Vector);
         return;
      end if;

      declare
         Command : constant String := Ada.Command_Line.Argument (1);
         Arguments : Argument_Vectors.Vector;
      begin

         if Command_Map.Contains (Command) then
            for I in 2 .. Ada.Command_Line.Argument_Count loop
               Arguments.Append (Ada.Command_Line.Argument (I));
            end loop;

            Command_Map.Element (Command).Execute (Arguments);
         else
            Alix.Commands.Help.Handler.Execute
              (Argument_Vectors.Empty_Vector);
         end if;
      end;

   end Execute;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      Command_Map.Insert ("configure", Configure.Handler);
      Command_Map.Insert ("help", Help.Handler);
      Command_Map.Insert ("install", Install.Handler);
      Command_Map.Insert ("update", Update.Handler);
   end Initialise;

   -----------------------
   -- Skip_Source_Clone --
   -----------------------

   function Skip_Source_Clone return Boolean is
   begin
      return False;
   end Skip_Source_Clone;

end Alix.Commands;
