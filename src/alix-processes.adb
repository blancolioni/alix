with Ada.Text_IO;

with GNAT.OS_Lib;

package body Alix.Processes is

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command : String) is
      use GNAT.OS_Lib;
      Args        : Argument_List_Access;
      Exit_Status : Integer;
   begin

      Ada.Text_IO.Put_Line (Command);

      --  Prepare the arguments. Splitting properly takes quotes into account.

      Args := Argument_String_To_List (Command);

      declare
         Qualified_Path : String_Access :=
                            Locate_Exec_On_Path (Args (Args'First).all);
      begin
         --  Spawn the command and wait for its possible completion

         Exit_Status := Spawn
           (Program_Name =>
              (if Qualified_Path /= null
               then Qualified_Path.all
               else Args (Args'First).all),
            Args         => Args (Args'First + 1 .. Args'Last));

         --  Free memory
         Free (Args);
         Free (Qualified_Path);
      end;

      if Exit_Status /= 0 then
         raise Constraint_Error with
           "spawn: could not execute " & Command;
      end if;

   end Spawn;

end Alix.Processes;
