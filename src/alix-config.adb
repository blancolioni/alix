with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with Tropos.Reader;

with Alix.Paths;

package body Alix.Config is

   Local_Config : Tropos.Configuration;

   function Substitute
     (Text      : String;
      Variable  : String;
      New_Value : String)
     return String;

   function Create_Project_And_Version
     (Project_Name : String;
      Version_Name : String)
      return String;

   -----------------------------
   -- Configuration_File_Path --
   -----------------------------

   function Configuration_File_Path
     (File_Name : String)
     return String
   is
   begin
      return Ada.Directories.Compose
        (Alix.Paths.Config_Path,
         File_Name);
   end Configuration_File_Path;

   --------------------------------
   -- Create_Project_And_Version --
   --------------------------------

   function Create_Project_And_Version
     (Project_Name : String;
      Version_Name : String)
      return String
   is
      Lower_Name : constant String :=
                     Ada.Characters.Handling.To_Lower (Project_Name);
   begin
      if Version_Name = ""
        or else Version_Name = "*"
        or else Version_Name = "any"
      then
         return Lower_Name;
      else
         return Lower_Name & "-" & Version_Name;
      end if;
   end Create_Project_And_Version;

   ---------
   -- Get --
   ---------

   function Get (Key : String) return String is
   begin
      return Local_Config.Get (Key, "");
   end Get;

   -----------------------
   -- Global_Build_Path --
   -----------------------

   function Global_Build_Path return String is
   begin
      return Ada.Directories.Compose
        (Local_Config.Get ("install_path"),
         "build");
   end Global_Build_Path;

   ----------------------
   -- Global_Exec_Path --
   ----------------------

   function Global_Exec_Path return String is
   begin
      return Ada.Directories.Compose (Global_Build_Path, "bin");
   end Global_Exec_Path;

   ------------------------
   -- Global_Object_Path --
   ------------------------

   function Global_Object_Path return String is
   begin
      return Ada.Directories.Compose (Global_Build_Path, "obj");
   end Global_Object_Path;

   function Global_Source_Path return String is
   begin
      return Ada.Directories.Compose
        (Local_Config.Get ("install_path"),
         "source");
   end Global_Source_Path;

   -----------------------
   -- Installation_Path --
   -----------------------

   function Installation_Path
     (Project_Name    : String;
      Version_Name    : String)
      return String
   is
      Install_Path : constant String :=
                       Local_Config.Get ("install_path");
      Source_Path  : constant String :=
                       Ada.Directories.Compose
                         (Install_Path,
                          "source");
      Project_And_Version : constant String :=
                              Create_Project_And_Version (Project_Name,
                                                          Version_Name);
      Project_Path : constant String :=
                       Ada.Directories.Compose
                         (Source_Path,
                          Project_And_Version);
   begin
      if not Ada.Directories.Exists (Install_Path) then
         Ada.Text_IO.Put_Line
           ("Creating alix directory " & Install_Path);
         Ada.Directories.Create_Directory (Install_Path);
         declare
            Build_Path : constant String :=
                           Ada.Directories.Compose
                             (Install_Path, "build");
            Bin_Path   : constant String :=
                           Ada.Directories.Compose
                             (Build_Path, "bin");
            Obj_Path   : constant String :=
                           Ada.Directories.Compose
                             (Build_Path, "obj");
         begin
            Ada.Directories.Create_Path (Bin_Path);
            Ada.Directories.Create_Path (Obj_Path);
         end;

      end if;

      if not Ada.Directories.Exists (Source_Path) then
         Ada.Text_IO.Put_Line
           ("Creating source directory " & Source_Path);
         Ada.Directories.Create_Directory (Source_Path);
      end if;

--        if not Ada.Directories.Exists (Project_Path) then
--           Ada.Text_IO.Put_Line
--             ("Creating project directory " & Project_Path);
--           Ada.Directories.Create_Directory (Project_Path);
--        end if;

      return Project_Path;

   end Installation_Path;

   -------------------------
   -- Project_Config_Path --
   -------------------------

   function Project_Config_Path
     (Project_Name    : String;
      Project_Version : String)
      return String
   is
      Install_Path : constant String :=
                       Local_Config.Get ("install_path");
      Config_Path  : constant String :=
                       Ada.Directories.Compose
                         (Install_Path,
                          "config");
      Config_Directory : constant String :=
                           Create_Project_And_Version (Project_Name,
                                                       Project_Version);
      Project_Path : constant String :=
                       Ada.Directories.Compose
                         (Config_Path, Config_Directory);
   begin
      Ada.Directories.Create_Path (Project_Path);
      return Project_Path;
   end Project_Config_Path;

   -------------------------------
   -- Pull_Script_Template_Path --
   -------------------------------

   function Pull_Script_Template_Path
     return String
   is
   begin
      return Configuration_File_Path ("pull_source_template.sh");
   end Pull_Script_Template_Path;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Local_Config :=
        Tropos.Reader.Read_Config
        (Ada.Directories.Compose
           (Alix.Paths.Config_Path, "alix.config"));
   end Read_Config;

   ----------------
   -- Server_URL --
   ----------------

   function Server_URL (Project_Name : String;
                        Version_Name : String)
                       return String
   is
      Base_Path : constant String :=
        Local_Config.Get ("server");
   begin
      return Substitute
        (Substitute (Base_Path, "project_name", Project_Name),
         "trunk_name", Version_Name);
   end Server_URL;

   --------------------------
   -- Standard_Replacement --
   --------------------------

   function Standard_Replacement
     (Project_Name    : String;
      Project_Version : String)
     return WL.Text.Text_Replacement
   is
      use WL.Text;
   begin
      return Result : Text_Replacement do
         Add (Result, "Project", Project_Name);
         Add (Result, "Version", Project_Version);
         Add (Result, "Temp_Folder", Local_Config.Get ("temp_path"));
         Add (Result, "Server", Server_URL (Project_Name, Project_Version));
      end return;
   end Standard_Replacement;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Text      : String;
      Variable  : String;
      New_Value : String)
     return String
   is
   begin
      for I in Text'First .. Text'Last - Variable'Length - 1 loop
         if Text (I .. I + Variable'Length + 1)
           = '{' & Variable & '}'
         then
            return Text (Text'First .. I - 1)
              & New_Value
              & Substitute (Text (I + Variable'Length + 2 .. Text'Last),
                            Variable,
                            New_Value);
         end if;
      end loop;
      return Text;
   end Substitute;

   -------------------------
   -- Temporary_File_Path --
   -------------------------

   function Temporary_File_Path
     (File_Name : String)
     return String
   is
   begin
      return Ada.Directories.Compose
        (Local_Config.Get ("temp_path"),
         File_Name);
   end Temporary_File_Path;

end Alix.Config;
