with Ada.Characters.Handling;
with Ada.Directories;

with Alix.Config;

package body Alix.Status is

   function Version_To_Ada_Identifier
     (Version : Alix.Versions.Version_Number)
      return String;

   --------------------------
   -- Get_GPR_Project_Name --
   --------------------------

   function Get_GPR_Project_Name
     (Project_Name : String;
      Version      : Alix.Versions.Version_Number)
      return String
   is
      Base_Name : constant String :=
                    Ada.Characters.Handling.To_Lower (Project_Name);
   begin
      if Version = "any"
        or else Version = "Trunk"
        or else Version = ""
      then
         return Base_Name;
      else
         return Base_Name & "_" & Version_To_Ada_Identifier (Version);
      end if;
   end Get_GPR_Project_Name;

   ----------------------
   -- GPR_Project_Path --
   ----------------------

   function GPR_Project_Path
     (Project_Name    : String;
      Project_Version : String)
      return String
   is
      Install_Path : constant String :=
                       Alix.Config.Get ("install_path");
      Project_Path : constant String :=
                       Ada.Directories.Compose
                         (Install_Path,
                          Get_GPR_Project_Name (Project_Name, Project_Version)
                          & ".gpr");
   begin
      return Project_Path;
   end GPR_Project_Path;

   --------------------------
   -- Get_Matching_Version --
   --------------------------

--     function Get_Matching_Version
--       (Project_Name     : String;
--        Version_Template : Alix.Versions.Version_Number)
--        return Alix.Versions.Version_Number
--     is
--     begin
--        if not Have_Package_Config then
--           Package_Config :=
--             Tropos.Reader.Read_Config
--               (Alix.Config.Configuration_File_Path ("packages.alix"));
--           Have_Package_Config := True;
--        end if;
--
--        declare
--           Vs : constant Tropos.Configuration :=
--             Package_Config.Child (Project_Name);
--           It : Tropos.Cursor := Vs.First;
--        begin
--           while Tropos.Has_Element (It) loop
--
--              if Alix.Versions.Match_Versions
--                (Version  => Tropos.Element (It).Config_Name,
--                 Template => Version_Template)
--              then
--                 return Tropos.Element (It).Config_Name;
--              end if;
--
--              Tropos.Next (It);
--           end loop;
--        end;
--
--        return "";
--
--     end Get_Matching_Version;

   ------------------
   -- Is_Installed --
   ------------------

   function Is_Installed
     (Project_Name : String;
      Version      : Alix.Versions.Version_Number)
      return Boolean
   is
      Project_Path : constant String :=
                      Alix.Config.Installation_Path (Project_Name, Version);
   begin
      return Ada.Directories.Exists (Project_Path);
   end Is_Installed;

   ------------------------------
   -- Latest_Installed_Version --
   ------------------------------

   function Latest_Installed_Version
     (Project_Name : String)
      return Alix.Versions.Version_Number
   is
      pragma Unreferenced (Project_Name);
   begin
      return "Trunk";
   end Latest_Installed_Version;

   -------------------------------
   -- Version_To_Ada_Identifier --
   -------------------------------

   function Version_To_Ada_Identifier
     (Version : Alix.Versions.Version_Number)
      return String
   is
      Result : String := Version;
   begin
      for I in Result'Range loop
         if Result (I) = '.' or else Result (I) = '-' then
            Result (I) := '_';
         end if;
      end loop;
      return Result;
   end Version_To_Ada_Identifier;

end Alix.Status;
