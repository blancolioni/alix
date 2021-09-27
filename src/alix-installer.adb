with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;

with WL.Text;

with Tropos.Reader;

with Alix.Commands;
with Alix.Config;
with Alix.Directories;
with Alix.Paths;
with Alix.Projects;
with Alix.Processes;
with Alix.Status;
with Alix.Versions;

package body Alix.Installer is

   function Pull_Source
     (Project_Name    : String;
      Repository      : String;
      Project_Version : String)
      return String;
   pragma Unreferenced (Pull_Source);

   function Copy_Source
     (Project_Name    : String;
      Project_Version : String)
      return String;

   pragma Unreferenced (Copy_Source);

   function Read_Alix_File (Installation_Path : String)
                             return Tropos.Configuration;

   procedure Write_Project_File
     (Project_Name      : String;
      Project_File_Path : String;
      Source_Directory  : String;
      Build_Base        : String;
      Project_Config    : Tropos.Configuration;
      Alix_Config       : Tropos.Configuration);
   --  Create a project file Project_Name in the directory given
   --  by Project_File_Path.  Source_Directory is the base for
   --  source paths given by the Project_Config configuration
   --  settings.  The build objects will be placed into
   --  Build_Base/obj, and any binaries into Build_Base/bin.
   --  Both are created if necessary

   procedure Write_Config_Path_Unit
     (Project_Name     : String;
      Project_Version  : String;
      Source_Directory : String;
      Config_Directory : String;
      Config           : Tropos.Configuration);
   --  If Config contains a config path element, write a corresponding
   --  unit which names the config path defined by
   --  Alix.Config.Project_Config_Path
   --
   --     package {Unit_Name} is
   --        Config_Path : constant String := "{Config_Path}";
   --     end {Unit_Name};

   procedure Build_Project (GPR_Project_Path : String);
   --  Launch gnatmake to build the project found at GPR_Project_Path

   procedure Check_Dependencies (Config : Tropos.Configuration);
   --  Check the alix config file for projects we depend on.  If any
   --  dependencies are not installed, attempt to install them.  This
   --  might cause further dependency checks and installations.

   -------------------
   -- Build_Project --
   -------------------

   procedure Build_Project (GPR_Project_Path : String) is
   begin
      Ada.Text_IO.Put_Line ("Building: " & GPR_Project_Path);
      Alix.Processes.Spawn
        (Alix.Config.Get ("gnatmake") & " -P " & GPR_Project_Path);
   end Build_Project;

   ------------------------
   -- Check_Dependencies --
   ------------------------

   procedure Check_Dependencies (Config : Tropos.Configuration) is

      procedure Check_Dependency (It : Tropos.Cursor);

      ----------------------
      -- Check_Dependency --
      ----------------------

      procedure Check_Dependency (It : Tropos.Cursor) is
         Dep_Config  : constant Tropos.Configuration :=
                         Tropos.Element (It);
         Project_Dep  : constant String :=
                         Dep_Config.Get ("project");
         Version_Dep  : constant String :=
           Dep_Config.Get ("version", "");
         External_Dep : constant Boolean := Dep_Config.Get ("external");
      begin

         if External_Dep then
            Ada.Text_IO.Put_Line ("external dependency: "
                                    & Project_Dep);
         else

            declare
               Project_Version : constant Alix.Versions.Version_Number :=
                                   Alix.Projects.Get_Matching_Version
                                     (Project_Dep, Version_Dep);
            begin

               Ada.Text_IO.Put_Line ("dependency: "
                                     & Project_Dep & "-" & Project_Version);

               if not Alix.Status.Is_Installed
                 (Project_Dep, Project_Version)
               then
                  Install (Project_Dep, Project_Version);
               end if;
            end;
         end if;

      end Check_Dependency;

   begin
      Config.Iterate ("depend", Check_Dependency'Access);
   end Check_Dependencies;

   ---------------
   -- Configure --
   ---------------

   procedure  Configure
     (Directory : String;
      Mode      : String)
   is
      Config           : constant Tropos.Configuration :=
        Read_Alix_File (Directory);
      Alix_Config      : constant Tropos.Configuration :=
                           Tropos.Reader.Read_Config
                             (Alix.Paths.Config_Path
                              & "/alix.config");

      Project_Name     : constant String :=
        Config.Get ("project_name");
      Project_File_Name : constant String :=
        Ada.Characters.Handling.To_Lower (Project_Name) & ".gpr";
      Project_File_Path : constant String :=
        Ada.Directories.Compose
        (Directory, Project_File_Name);
   begin
      Ada.Text_IO.Put_Line ("Found alix file: " & Config.Config_Name);

      Check_Dependencies (Config);

      Write_Config_Path_Unit
        (Project_Name, "", Directory, "", Config);

      Write_Project_File
        (Project_Name      => Config.Get ("project_name"),
         Project_File_Path => Project_File_Path,
         Source_Directory  => ".",
         Project_Config    => Config,
         Alix_Config       => Alix_Config.Child (Mode),
         Build_Base        => "build");
   end Configure;

   -----------------
   -- Copy_Source --
   -----------------

   function Copy_Source
     (Project_Name    : String;
      Project_Version : String)
      return String
   is
      Project_Path : constant String :=
                       Alix.Config.Installation_Path (Project_Name,
                                                      Project_Version);
      Version_Path : constant String :=
                       Ada.Directories.Compose (Project_Path,
                                                Project_Name & "-"
                                                & Project_Version);
   begin

      if not Alix.Commands.Skip_Source_Clone then

         if Ada.Directories.Exists (Version_Path) then
            return Version_Path;
         end if;

         Ada.Directories.Create_Path (Project_Path);
         Ada.Directories.Set_Directory (Project_Path);

         Ada.Text_IO.Put_Line ("Fetching " & Project_Name
                               & "-" & Version_Path
                               & ".tar.gz ...");

         Alix.Processes.Spawn
           (Alix.Config.Get ("wget")
            & " "
            & Alix.Config.Get ("repository_url")
            & "/" & Project_Name
            & "-" & Project_Version
            & ".tar.gz");

         Ada.Text_IO.Put_Line ("Extracting " & Project_Name
                               & "-" & Version_Path
                               & ".tar.gz ...");

         Alix.Processes.Spawn
           (Alix.Config.Get ("tar")
            & " xfvz "
            & Project_Name
            & "-" & Project_Version
            & ".tar.gz");

      else
         Ada.Text_IO.Put_Line ("Skipping source clone");
      end if;

      return Version_Path;
   end Copy_Source;

   -------------
   -- Install --
   -------------

   procedure  Install (Project_Name     : String;
                       Version_Template : String := "*")
   is
   begin

      Ada.Text_IO.Put_Line ("Install: " & Project_Name);

      declare
         Project_Version  : constant String :=
                              Alix.Projects.Get_Matching_Version
                                (Project_Name,
                                 Version_Template);
         Source_Directory : constant String :=
                              Alix.Projects.Fetch_Project
                                (Project_Name, Version_Template);
         Config_Directory : constant String :=
                              Alix.Config.Project_Config_Path
                                (Project_Name,
                                 Project_Version);
         GPR_Project_Path : constant String :=
                              Alix.Status.GPR_Project_Path (Project_Name,
                                                            Project_Version);
         Config           : constant Tropos.Configuration :=
                              Read_Alix_File (Source_Directory);
         Alix_Config      : constant Tropos.Configuration :=
                              Tropos.Reader.Read_Config
                                (Alix.Paths.Config_Path
                                 & "/alix.config");
      begin
         Ada.Text_IO.Put_Line ("Source directory " & Source_Directory);
         Ada.Text_IO.Put_Line ("Config directory " & Config_Directory);
         Ada.Text_IO.Put_Line ("Found alix file: " & Config.Config_Name);

         Check_Dependencies (Config);

         Write_Config_Path_Unit (Project_Name     => Project_Name,
                                 Project_Version  => Project_Version,
                                 Source_Directory => Source_Directory,
                                 Config_Directory => Config_Directory,
                                 Config           => Config);

         Write_Project_File
           (Project_Name      => Project_Name,
            Project_File_Path => GPR_Project_Path,
            Source_Directory  => Source_Directory,
            Project_Config    => Config,
            Alix_Config       => Alix_Config.Child ("install"),
            Build_Base        => Alix.Config.Global_Build_Path);

         Build_Project (GPR_Project_Path);

         if Config.Contains ("main_unit") then
            Ada.Text_IO.Put_Line ("Installing executable");
            declare
               Exec_Ext   : constant String :=
                              Alix.Config.Get ("executable_extension");
               Build_Name : constant String :=
                              Config.Get ("main_unit")
                              & Exec_Ext;
               Exec_Name : constant String :=
                             (if Config.Contains ("exec_name")
                              then Config.Get ("exec_name")
                              else Config.Get ("main_unit"))
                 & Exec_Ext;
            begin
               Ada.Directories.Copy_File
                 (Ada.Directories.Compose
                    (Alix.Config.Global_Exec_Path, Build_Name),
                  Ada.Directories.Compose
                    (Alix.Config.Get ("exec_path"),
                     Exec_Name));
               GNAT.OS_Lib.Set_Executable
                  (Ada.Directories.Compose
                     (Alix.Config.Get ("exec_path"),
                     Exec_Name));
            end;
         end if;

         if Config.Contains ("config_dir") then
            declare
               Config_Path  : constant String :=
                                Alix.Config.Project_Config_Path
                                  (Project_Name,
                                   Project_Version);
               Local_Config : constant String :=
                                Alix.Directories.Compose_Directories
                                  (Source_Directory,
                                   Config.Get ("config_dir"));
            begin
               Ada.Text_IO.Put_Line ("Installing config to " & Config_Path);

               Ada.Directories.Create_Path (Config_Path);
               Alix.Directories.Copy_Directory
                 (Local_Config, Config_Path);
            end;
         end if;

         if Config.Contains ("post_install") then
            declare
               Command : constant String := Config.Get ("post_install");
               Replacement : WL.Text.Text_Replacement;
            begin
               WL.Text.Add
                 (Replacement, "exec_path", Alix.Config.Get ("exec_path"));

               Alix.Processes.Spawn
                 (WL.Text.Replace (Command, Replacement));
            end;
         end if;

      end;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "installation failed");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "  error was: "
            & Ada.Exceptions.Exception_Message (E));
   end Install;

   -----------------
   -- Pull_Source --
   -----------------

   function Pull_Source
     (Project_Name    : String;
      Repository      : String;
      Project_Version : String)
      return String
   is
      Project_Path : constant String :=
                       Alix.Config.Installation_Path (Project_Name,
                                                      Project_Version);
      Version_Path : constant String :=
                       Ada.Directories.Compose (Project_Path,
                                                Repository);
   begin

      if not Alix.Commands.Skip_Source_Clone then

         if Ada.Directories.Exists (Version_Path) then
            return Version_Path;
         end if;

         Ada.Directories.Set_Directory (Project_Path);

         if Project_Version = "*" then
            Alix.Processes.Spawn
              (Alix.Config.Get ("hg") & " clone "
               & Alix.Config.Server_URL (Project_Name, Project_Version));
         else
            Alix.Processes.Spawn
              (Alix.Config.Get ("hg") & " clone "
               & "-r " & Project_Version & " "
               & Alix.Config.Server_URL (Project_Name, Project_Version));
         end if;

      else
         Ada.Text_IO.Put_Line ("Skipping source clone");
      end if;

      return Version_Path;
   end Pull_Source;

   ---------------------
   -- Read_Alix_File --
   ---------------------

   function Read_Alix_File (Installation_Path : String)
                             return Tropos.Configuration
   is

      Config : Tropos.Configuration;
      Got_Config : Boolean := False;

      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -----------------
      -- Call_Reader --
      -----------------

      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
      begin
         if Got_Config then
            raise Constraint_Error
              with "multiple alix files in " & Installation_Path;
         end if;

         Got_Config := True;
         Config := Tropos.Reader.Read_Config (Full_Name (Directory_Entry));
      end Call_Reader;

   begin
      Ada.Directories.Search
        (Directory      => Installation_Path,
         Pattern        => "*.alix",
         Filter         => (Ada.Directories.Ordinary_File => True,
                            others => False),
         Process        => Call_Reader'Access);

      if Got_Config then
         return Config;
      else
         raise Constraint_Error
           with "cannot find an alix file in " & Installation_Path;
      end if;

   end Read_Alix_File;

   ----------------------------
   -- Write_Config_Path_Unit --
   ----------------------------

   procedure Write_Config_Path_Unit
     (Project_Name     : String;
      Project_Version  : String;
      Source_Directory : String;
      Config_Directory : String;
      Config           : Tropos.Configuration)
   is
   begin
      if Config.Contains ("path_unit") then
         declare
            use Ada.Text_IO;
            File : File_Type;
            Path_Unit_Config : constant Tropos.Configuration :=
              Config.Child ("path_unit");
            Config_Path      : constant String :=
                  (if Config_Directory /= ""
                   then Config_Directory
                   elsif Project_Version = ""
                   then Ada.Directories.Compose
                     (Source_Directory,
                      Config.Get ("config_dir"))
                   else Alix.Config.Project_Config_Path
                     (Project_Name,
                      Project_Version));
         begin
            Create
              (File, Out_File,
               Alix.Directories.Compose_Directories
                 (Source_Directory,
                  Path_Unit_Config.Get ("path")));

            Put_Line (File,
                      "package " & Path_Unit_Config.Get ("unit")
                      & " is");

            New_Line (File);

            Put_Line (File, "   Config_Path : constant String :=");
            Put_Line (File,
                      "     """ & Config_Path & """;");
            New_Line (File);
            Put_Line (File, "   function Config_File");
            Put_Line (File, "     (File_Path : String)");
            Put_Line (File, "     return String");
            Put_Line (File, "   is (Config_Path & ""/"" & File_Path);");

            New_Line (File);

            Put_Line (File,
                      "end " & Path_Unit_Config.Get ("unit")
                        & ";");
            Close (File);
         end;
      end if;
   end Write_Config_Path_Unit;

   ------------------------
   -- Write_Project_File --
   ------------------------

   procedure Write_Project_File
     (Project_Name      : String;
      Project_File_Path : String;
      Source_Directory  : String;
      Build_Base        : String;
      Project_Config    : Tropos.Configuration;
      Alix_Config       : Tropos.Configuration)
   is
      pragma Unreferenced (Project_Name);
      use Ada.Text_IO;
      File : File_Type;

      First_Item : Boolean;
      Path_Prefix : Boolean;

      procedure Write_List (It : Tropos.Cursor);
      procedure Write_Single_Line_List (It : Tropos.Cursor);
      procedure Write_Project_With (It : Tropos.Cursor);

      procedure Iterate_Config_File
        (Field_Name : String;
         Process    : not null access
           procedure (It : Tropos.Cursor));

      -------------------------
      -- Iterate_Config_File --
      -------------------------

      procedure Iterate_Config_File
        (Field_Name : String;
         Process    : not null access
           procedure (It : Tropos.Cursor))
      is
         procedure Process_Guarded_Config (It : Tropos.Cursor);

         ----------------------------
         -- Process_Guarded_Config --
         ----------------------------

         procedure Process_Guarded_Config (It : Tropos.Cursor) is

            Success : Boolean := True;

            procedure Check (Name  : String;
                             Value : String);

            -----------
            -- Check --
            -----------

            procedure Check (Name  : String;
                             Value : String)
            is
            begin
               Success := Success
                 and then Alix.Config.Has_Value (Name, Value);
            end Check;

         begin
            Tropos.Element (It).Iterate_Attributes (Check'Access);
            if Success then
               Tropos.Element (It).Iterate (Field_Name, Process);
            end if;
         end Process_Guarded_Config;

      begin
         Project_Config.Iterate (Field_Name, Process);
         Project_Config.Iterate ("when", Process_Guarded_Config'Access);
      end Iterate_Config_File;

      ----------------
      -- Write_List --
      ----------------

      procedure Write_List (It : Tropos.Cursor) is
      begin
         if not First_Item then
            Put_Line (File, ",");
         else
            First_Item := False;
         end if;

         declare
            Name : constant String := Tropos.Element (It).Value;
            Value : constant String :=
                      (if Path_Prefix
                       then Alix.Directories.Compose_Directories
                         (Source_Directory, Name)
                       else Name);
         begin
            Put (File,
                 "     """ & Value & """");
         end;
      end Write_List;

      ------------------------
      -- Write_Project_With --
      ------------------------

      procedure Write_Project_With (It : Tropos.Cursor) is
         Depend_Config : constant Tropos.Configuration :=
                           Tropos.Element (It);
         Project_Dep   : constant String :=
                           Depend_Config.Get ("project");
         Version_Dep   : constant String :=
                           Depend_Config.Get ("version", "");
         External_Dep : constant Boolean :=
           Depend_Config.Get ("external");
      begin
         if External_Dep then
            Put_Line (File,
                      "with """
                        & Ada.Characters.Handling.To_Lower (Project_Dep)
                        & """;");
         else
            Put_Line
              (File,
               "with """
               & Alix.Status.Get_GPR_Project_Name
                 (Project_Dep,
                  Alix.Projects.Get_Matching_Version
                    (Project_Dep, Version_Dep))
               & """;");
         end if;
      end Write_Project_With;

      ----------------------------
      -- Write_Single_Line_List --
      ----------------------------

      procedure Write_Single_Line_List (It : Tropos.Cursor) is
      begin
         if not First_Item then
            Put (File, ", ");
         else
            First_Item := False;
         end if;

         Put (File, """" & Tropos.Element (It).Value & """");
      end Write_Single_Line_List;

   begin

      if Project_Config.Contains ("main_unit") then
         Ada.Directories.Create_Path
           (Alix.Directories.Compose_Directories
              (Build_Base, "bin"));
      end if;

      Ada.Directories.Create_Path
        (Alix.Directories.Compose_Directories
           (Build_Base, "obj"));

      Create (File, Out_File, Project_File_Path);

      Iterate_Config_File ("depend", Write_Project_With'Access);

      Put_Line (File,
                "project "
                & Ada.Directories.Base_Name (Project_File_Path)
                & " is");
      New_Line (File);
      Put_Line (File, "   for Source_Dirs use (");
      First_Item  := True;
      Path_Prefix := True;
      Iterate_Config_File ("source_dir", Write_List'Access);
      Path_Prefix := False;
      Put_Line (File, ");");
      New_Line (File);

      Put_Line (File,
                "   for Object_Dir use """
                & Ada.Directories.Compose (Build_Base, "obj")
                & """;");

      if Project_Config.Contains ("main_unit") then
         Put_Line (File,
                   "   for Exec_Dir use """
                   & Ada.Directories.Compose (Build_Base, "bin")
                   & """;");
         Put_Line (File,
                   "   for Main use (");
         First_Item := True;
         Iterate_Config_File ("main_unit", Write_List'Access);
         Put_Line (File, ");");
      end if;
      New_Line (File);

      Put_Line (File,
                "   package Builder is");
      Put (File,
                "      for Default_Switches (""ada"") use (");
      First_Item := True;
      Alix_Config.Iterate ("builder_option",
                           Write_Single_Line_List'Access);
      Put_Line (File, ");");
      Put_Line (File,
                "   end Builder;");
      New_Line (File);

      Put_Line (File,
                "   package Compiler is");
      Put (File,
                "      for Default_Switches (""ada"") use (");
      First_Item := True;
      Alix_Config.Iterate ("compiler_option",
                           Write_Single_Line_List'Access);
      Put_Line (File, ");");
      Put_Line (File,
                "   end Compiler;");

      New_Line (File);

      Put_Line (File,
                "   package Linker is");
      Put (File,
                "      for Default_Switches (""ada"") use (");
      First_Item := True;
      Iterate_Config_File ("linker_option",
                              Write_Single_Line_List'Access);
      Put_Line (File, ");");
      Put_Line (File,
                "   end Linker;");

      New_Line (File);

      if Project_Config.Contains ("language") then
         First_Item := True;
         Put (File, "   for Languages use (");
         Project_Config.Iterate ("language",
                                 Write_Single_Line_List'Access);
         Put_Line (File, ");");
         New_Line (File);
      end if;

      Put_Line (File,
                "end "
                & Ada.Directories.Base_Name (Project_File_Path)
                & ";");

      Close (File);

   end Write_Project_File;

end Alix.Installer;
