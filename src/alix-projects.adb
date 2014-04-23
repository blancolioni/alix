with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with Tropos.Reader;

with Alix.Commands;
with Alix.Config;
with Alix.Paths;
with Alix.Processes;
with Alix.Versions;

package body Alix.Projects is

   Project_Config_Path : constant String :=
                           Ada.Directories.Compose
                             (Alix.Paths.Config_Path,
                              "projects.alix");

   Project_Config : Tropos.Configuration;

   function Get_Project_Config
     (Project_Name : String)
      return Tropos.Configuration;
   --  Find the child of Project_Config which matches Project_Name.
   --  Not case sensitive
   --  If the project name is not found, raise Project_Not_Found

   function Get_Matching_Version
     (Project_Config : Tropos.Configuration;
      Version_Template : String)
      return String;
   --  Return the first version number found in Project_Config which
   --  matches Version_Template
   --  If Version_Template is "*" or "any", just return an empty string

   function Clone_Source
     (Project_Name    : String;
      Repository_URL  : String;
      Project_Version : String)
      return String;
   --  Use an hg clone command to copy source from the repository.
   --  If Project_Version is not empty, use the -r option to clone
   --  a particular version.
   --  If the project directory already exists, use a pull followed by
   --  an update.
   --  Return the source directory path

   function Copy_Source
     (Project_Name    : String;
      Tar_Archive_URL : String;
      Project_Version : String)
      return String;
   --  Use wget to pull a tar archive from the given url, extract it,
   --  and return the path to the new source directory.
   --  Project_Version must be explicit

   ------------------
   -- Clone_Source --
   ------------------

   function Clone_Source
     (Project_Name    : String;
      Repository_URL  : String;
      Project_Version : String)
      return String
   is
      Project_Path : constant String :=
                       Alix.Config.Installation_Path (Project_Name,
                                                      Project_Version);
   begin

      if Ada.Directories.Exists (Project_Path) then
         if Project_Version = ""
           or else Project_Version = "*"
           or else Project_Version = "any"
         then
            Ada.Text_IO.Put_Line
              ("Updating existing source in " & Project_Path);
            Ada.Directories.Set_Directory
              (Project_Path);
            Alix.Processes.Spawn
              (Alix.Config.Get ("hg") & " pull");
            Alix.Processes.Spawn
              (Alix.Config.Get ("hg") & " update");
         else
            Ada.Text_IO.Put_Line
              ("Not updating version " & Project_Version);
         end if;
      else
         if not Alix.Commands.Skip_Source_Clone then

            if Project_Version = "" then
               Alix.Processes.Spawn
                 (Alix.Config.Get ("hg") & " clone "
                  & Repository_URL & " "
                  & Project_Path);
            else
               Alix.Processes.Spawn
                 (Alix.Config.Get ("hg") & " clone "
                  & "-r " & Project_Version & " "
                  & Repository_URL & " "
                  & Project_Path);
            end if;

         end if;
      end if;

      return Project_Path;
   end Clone_Source;

   -----------------
   -- Copy_Source --
   -----------------

   function Copy_Source
     (Project_Name    : String;
      Tar_Archive_URL : String;
      Project_Version : String)
      return String
   is
      pragma Unreferenced (Project_Name);
      pragma Unreferenced (Tar_Archive_URL);
      pragma Unreferenced (Project_Version);
   begin
      return "";
   end Copy_Source;

   -------------------
   -- Fetch_Project --
   -------------------

   function Fetch_Project
     (Project_Name     : String;
      Version_Template : String)
      return String
   is
      Config : constant Tropos.Configuration :=
                 Get_Project_Config (Project_Name);
      Project_Version : constant String :=
                          Get_Matching_Version
                            (Config,
                             Version_Template);
   begin
      if Config.Contains ("hg") then
         return Clone_Source (Config.Config_Name,
                              Config.Get ("hg"),
                              Project_Version);
      elsif Config.Contains ("tgz") then
         return Copy_Source (Config.Config_Name,
                             Config.Get ("tgz"),
                             Project_Version);
      else
         raise Project_Not_Found
           with "error in projects.alix file for project " & Project_Name
             & ": no project location found";
      end if;
   end Fetch_Project;

   --------------------------
   -- Get_Matching_Version --
   --------------------------

   function Get_Matching_Version
     (Project_Name     : String;
      Version_Template : String)
      return String
   is
      Config : constant Tropos.Configuration :=
                 Get_Project_Config (Project_Name);
      Project_Version : constant String :=
                          Get_Matching_Version
                            (Config,
                             Version_Template);
   begin
      return Project_Version;
   end Get_Matching_Version;

   --------------------------
   -- Get_Matching_Version --
   --------------------------

   function Get_Matching_Version
     (Project_Config : Tropos.Configuration;
      Version_Template : String)
      return String
   is
      use Tropos;
      It : Cursor := Project_Config.First;
   begin

      if Version_Template = ""
        or else Version_Template = "*"
        or else Version_Template = "any"
      then
         return "";
      end if;

      while Has_Element (It) loop
         declare
            Child : constant Configuration :=
                      Element (It);
         begin
            if Child.Config_Name = "version"
              and then Alix.Versions.Match_Versions (Child.Value,
                                                     Version_Template)
            then
               return Child.Value;
            end if;
         end;
         Next (It);
      end loop;

      raise Project_Not_Found with
        "Project " & Project_Config.Config_Name
          & " has no versions which match "
          & Version_Template;
   end Get_Matching_Version;

   ------------------------
   -- Get_Project_Config --
   ------------------------

   function Get_Project_Config
     (Project_Name : String)
      return Tropos.Configuration
   is
      Lower_Name : constant String :=
                     Ada.Characters.Handling.To_Lower (Project_Name);
   begin
      return Project_Config.Child (Lower_Name);
   exception
      when others =>
         raise Project_Not_Found with
         Project_Name & " not found in "
           & Project_Config_Path;
   end Get_Project_Config;

begin
   Project_Config :=
     Tropos.Reader.Read_Config (Project_Config_Path);
end Alix.Projects;
