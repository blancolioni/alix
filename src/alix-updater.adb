with Ada.Directories;

with Alix.Config;
with Alix.Paths;
with Alix.Processes;

package body Alix.Updater is

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Ada.Directories.Set_Directory (Alix.Paths.Config_Path);
      Ada.Directories.Delete_File ("packages.alix");
      Alix.Processes.Spawn
        (Alix.Config.Get ("wget") & " "
         & Alix.Config.Get ("repository_url") & "/packages.alix");
   end Update;

end Alix.Updater;
