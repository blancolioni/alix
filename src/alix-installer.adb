with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;

with Tropos.Reader;

with Alix.Commands;
with Alix.Config;
with Alix.Directories;
with Alix.Status;
with Alix.Versions;

package body Alix.Installer is

   procedure Spawn (Command : String);

   function Pull_Source
     (Project_Name    : String;
      Project_Version : String)
      return String;

   function Read_Alix_File (Installation_Path : String)
                             return Tropos.Configuration;

   procedure Write_Project_File
     (Project_Name      : String;
      Project_File_Path : String;
      Source_Directory  : String;
      Build_Base        : String;
      Project_Config    : Tropos.Configuration);
   --  Create a project file Project_Name in the directory given
   --  by Project_File_Path.  Source_Directory is the base for
   --  source paths given by the Project_Config configuration
   --  settings.  The build objects will be placed into
   --  Build_Base/obj, and any binaries into Build_Base/bin.
   --  Both are created if necessary

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
      Spawn (Alix.Config.Get ("gnatmake") & " -P " & GPR_Project_Path);
   end Build_Project;

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

            Ada.Text_IO.Put_Line ("dependency: "
                                    & Project_Dep & "-" & Version_Dep);

            if not Alix.Status.Is_Installed (Project_Dep, Version_Dep) then
               declare
                  Project_Version : constant Alix.Versions.Version_Number :=
                    Alix.Status.Get_Matching_Version
                    (Project_Dep, Version_Dep);
               begin
                  if Project_Version /= "" then
                     Install (Project_Dep, Project_Version);
                  else
                     raise Constraint_Error with
                       "Cannot find package "
                       & Project_Dep & "-" & Project_Version;
                  end if;
               end;
            end if;
         end if;

      end Check_Dependency;

   begin
      Config.Iterate ("depend", Check_Dependency'Access);
   end Check_Dependencies;

   ---------------
   -- Configure --
   ---------------

   procedure  Configure (Directory : String) is
         Config           : constant Tropos.Configuration :=
                           Read_Alix_File (Directory);
   begin
      Ada.Text_IO.Put_Line ("Found alix file: " & Config.Config_Name);
      Check_Dependencies (Config);
      Write_Project_File (Project_Name      => Config.Get ("project_name"),
                          Project_File_Path =>
                            Ada.Directories.Compose
                              (Directory,
                               Ada.Characters.Handling.To_Lower
                                 (Config.Get ("project_name"))
                                 & ".gpr"),
                          Source_Directory  => ".",
                          Project_Config    => Config,
                          Build_Base        => "build");
   end Configure;

   -------------
   -- Install --
   -------------

   procedure  Install (Project_Name    : String;
                       Project_Version : String := "Trunk")
   is
   begin
      Ada.Text_IO.Put_Line ("Install: " & Project_Name);
      Ada.Text_IO.Put_Line ("Server URL: " &
                              Alix.Config.Server_URL (Project_Name,
                                                       Project_Version));

      declare
         Source_Directory : constant String :=
                              Pull_Source (Project_Name,
                                           Project_Version);
         GPR_Project_Path : constant String :=
                              Alix.Config.GPR_Project_Path (Project_Name);
         Config           : constant Tropos.Configuration :=
                              Read_Alix_File (Source_Directory);
      begin
         Ada.Text_IO.Put_Line ("Source pulled to " & Source_Directory);
         Ada.Text_IO.Put_Line ("Found alix file: " & Config.Config_Name);

         Check_Dependencies (Config);

         Write_Project_File
           (Project_Name      => Project_Name,
            Project_File_Path => GPR_Project_Path,
            Source_Directory  => Source_Directory,
            Project_Config    => Config,
            Build_Base        => Alix.Config.Global_Build_Path);

         if Config.Contains ("path_unit") then
            declare
               use Ada.Text_IO;
               File : File_Type;
               Path_Unit_Config : constant Tropos.Configuration :=
                                    Config.Child ("path_unit");
               Config_Path      : constant String :=
                                    Alix.Config.Project_Config_Path
                                      (Project_Name,
                                       Project_Version);
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
               Put_Line (File,
                         "end " & Path_Unit_Config.Get ("unit")
                         & ";");
               Close (File);
            end;
         end if;


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
      Project_Version : String)
      return String
   is
      Project_Path : constant String :=
                      Alix.Config.Installation_Path (Project_Name);
      Version_Path : constant String :=
                       Ada.Directories.Compose (Project_Path,
                                                Project_Version);
   begin

      if not Alix.Commands.Skip_Source_Clone then

         if Ada.Directories.Exists (Version_Path) then
            return Version_Path;
         end if;

         Ada.Directories.Set_Directory (Project_Path);

         Spawn (Alix.Config.Get ("hg") & " clone "
                & Alix.Config.Server_URL (Project_Name, Project_Version));
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

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command : String) is
      use GNAT.OS_Lib;
      Args        : Argument_List_Access;
      Exit_Status : Integer;
   begin
      --  Prepare the arguments. Splitting properly takes quotes into account.

      Args := Argument_String_To_List (Command);

      --  Spawn the command and wait for its possible completion

      Exit_Status := Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last));

      --  Free memory
      Free (Args);

      if Exit_Status /= 0 then
         raise Constraint_Error with
           "spawn: could not execute " & Command;
      end if;

   end Spawn;

   ------------------------
   -- Write_Project_File --
   ------------------------

   procedure Write_Project_File
     (Project_Name      : String;
      Project_File_Path : String;
      Source_Directory  : String;
      Build_Base        : String;
      Project_Config    : Tropos.Configuration)
   is
      use Ada.Text_IO;
      File : File_Type;

      First_Item : Boolean;
      Path_Prefix : Boolean;

      procedure Write_List (It : Tropos.Cursor);
      procedure Write_Single_Line_List (It : Tropos.Cursor);
      procedure Write_Project_With (It : Tropos.Cursor);

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
            Put_Line (File,
                      "with """
                        & Alix.Status.Get_GPR_Project_Name
                        (Project_Dep, Version_Dep)
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

      Project_Config.Iterate ("depend", Write_Project_With'Access);

      Put_Line (File, "project " & Project_Name & " is");
      New_Line (File);
      Put_Line (File, "   for Source_Dirs use (");
      First_Item  := True;
      Path_Prefix := True;
      Project_Config.Iterate ("source_dir", Write_List'Access);
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
         Project_Config.Iterate ("main_unit", Write_List'Access);
         Put_Line (File, ");");
      end if;
      New_Line (File);

      Put_Line (File,
                "   package Builder is");
      Put (File,
                "      for Default_Switches (""ada"") use (");
      First_Item := True;
      Project_Config.Iterate ("builder_option",
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
      Project_Config.Iterate ("compiler_option",
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
      Project_Config.Iterate ("linker_option",
                              Write_Single_Line_List'Access);
      Put_Line (File, ");");
      Put_Line (File,
                "   end Linker;");

      New_Line (File);

      Put_Line (File, "end " & Project_Name & ";");
      Close (File);

   end Write_Project_File;

end Alix.Installer;
