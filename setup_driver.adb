with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with Alix;
with Alix.Git;
with Alix.Processes;

procedure Setup_Driver is

   Windows_Commanded : constant Boolean :=
     Ada.Command_Line.Argument_Count > 0
     and then Ada.Command_Line.Argument (1) = "windows";

   Posix_Commanded   : constant Boolean :=
     Ada.Command_Line.Argument_Count > 0
     and then Ada.Command_Line.Argument (1) = "posix";

   Windows_Target : Boolean := False;
   Posix_Target   : Boolean := False;

   function Executable_Name
     (Base_Name : String)
      return String
   is (if Windows_Target
       then Base_Name & ".exe"
       else Base_Name);

   function To_Internal_Path (External_Path : String) return String;

   function Default_Install_Directory return String;

   function Get_Prompted_Text
     (Prompt  : String;
      Default : String)
      return String;

   procedure Create_Paths_Package
     (Installation_Path : String);

   procedure Create_Config_File
     (Installation_Path : String;
      Executable_Path   : String;
      Alix_Config_Path  : String);

   ------------------------
   -- Create_Config_File --
   ------------------------

   procedure Create_Config_File
     (Installation_Path : String;
      Executable_Path   : String;
      Alix_Config_Path  : String)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      Target_File : File_Type;
      Base_File   : File_Type;
   begin
      Create (Target_File, Out_File,
              Compose (Alix_Config_Path, "alix.config"));
      Set_Output (Target_File);
      Put_Line ("install_path = """
                & To_Internal_Path (Installation_Path)
                & """");
      Put_Line ("exec_path = """
                & To_Internal_Path (Executable_Path)
                & """");
      if Windows_Target then
         Put_Line ("executable_extension = "".exe""");
      end if;

      Open (Base_File, In_File, "config/alix.config");
      Set_Input (Base_File);

      while not End_Of_File loop
         Put_Line (Get_Line);
      end loop;

      Set_Input (Standard_Input);
      Close (Base_File);

      Set_Output (Standard_Output);
      Close (Target_File);

   end Create_Config_File;

   --------------------------
   -- Create_Paths_Package --
   --------------------------

   procedure Create_Paths_Package
     (Installation_Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, "src/alix-paths.ads");
      Set_Output (File);
      Put_Line ("package Alix.Paths is");
      Put_Line ("   Config_Path : constant String :=");
      Put_Line ("     """ & Installation_Path & """;");
      Put_Line ("end Alix.Paths;");
      Set_Output (Standard_Output);
      Close (File);
   end Create_Paths_Package;

   -------------------------------
   -- Default_Install_Directory --
   -------------------------------

   function Default_Install_Directory return String is
      use Ada.Environment_Variables;
      Home       : constant String := Value ("HOME", "");
      Home_Drive : constant String := Value ("HOMEDRIVE", "");
      Home_Path  : constant String := Value ("HOMEPATH", "");
      Windows    : constant Boolean :=
        Windows_Commanded
        or else (not Posix_Commanded
                 and then Home_Drive /= ""
                 and then Home_Path /= ""
                 and then Ada.Strings.Fixed.Index (Home_Path, "\") > 0);
   begin
      Windows_Target := Windows;
      Posix_Target := not Windows;

      if Windows then
         return Home_Drive & Home_Path & "\alix";
      else
         return Home & "/.alix";
      end if;
   end Default_Install_Directory;

   -----------------------
   -- Get_Prompted_Text --
   -----------------------

   function Get_Prompted_Text
     (Prompt  : String;
      Default : String)
      return String
   is
   begin
      Ada.Text_IO.Put (Prompt);
      if Default /= "" then
         Ada.Text_IO.Put (" [" & Default & "]");
      end if;
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Flush;

      declare
         Response : constant String := Ada.Text_IO.Get_Line;
      begin
         return (if Response = "" then Default else Response);
      end;
   end Get_Prompted_Text;

   ----------------------
   -- To_Internal_Path --
   ----------------------

   function To_Internal_Path (External_Path : String) return String is
   begin
      return Path : String := External_Path do
         for Ch of Path loop
            if Ch = '\' then
               Ch := '/';
            end if;
         end loop;
      end return;
   end To_Internal_Path;

begin
   Ada.Text_IO.Put_Line ("Alix version " & Alix.Version_Name);

   declare
      Alix_Folder : constant String :=
        Get_Prompted_Text
          (Prompt  => "installation folder",
           Default => Default_Install_Directory);
      Bin_Folder    : constant String :=
        Get_Prompted_Text
          (Prompt => "bin folder",
           Default =>
             Ada.Directories.Compose (Alix_Folder, "bin"));
      Build_Folder    : constant String :=
        Ada.Directories.Compose (Alix_Folder, "build");
      Config_Folder   : constant String :=
        Ada.Directories.Compose (Alix_Folder, "config");
      Alix_Config   : constant String :=
        Ada.Directories.Compose (Config_Folder, "alix");
   begin
      Ada.Text_IO.Put_Line ("installing to: " & Alix_Folder);
      if Ada.Directories.Exists ("dependencies") then
         Ada.Text_IO.Put_Line ("deleting old dependencies");
         Ada.Directories.Delete_Tree ("dependencies");
      end if;
      Ada.Directories.Create_Directory ("dependencies");
      Ada.Text_IO.Put_Line ("getting dependencies");
      Alix.Git.Clone
        (Local_Path      => "dependencies/wlib",
         Repository_Path => "https://github.com/blancolioni/wlib.git",
         Branch_Name     => "alix");
      Alix.Git.Clone
        (Local_Path      => "dependencies/tropos",
         Repository_Path => "https://github.com/blancolioni/tropos.git",
         Branch_Name     => "alix");
      Alix.Git.Clone
        (Local_Path      => "dependencies/alix-projects",
         Repository_Path =>
           "https://github.com/blancolioni/alix-projects.git");

      Create_Paths_Package (Alix_Folder & "/config/alix");

      if not Ada.Directories.Exists (Alix_Folder) then
         Ada.Directories.Create_Directory (Alix_Folder);
      end if;

      if not Ada.Directories.Exists (Config_Folder) then
         Ada.Directories.Create_Directory (Config_Folder);
      end if;

      if not Ada.Directories.Exists (Alix_Config) then
         Ada.Directories.Create_Directory (Alix_Config);
      end if;

      if not Ada.Directories.Exists (Bin_Folder) then
         Ada.Directories.Create_Directory (Bin_Folder);
      end if;

      if not Ada.Directories.Exists (Build_Folder) then
         Ada.Directories.Create_Directory (Build_Folder);
      end if;

      Ada.Directories.Copy_File
        (Source_Name => "dependencies/alix-projects/projects.alix",
         Target_Name => Alix_Config & "/projects.alix");

      Create_Config_File
        (Installation_Path => Alix_Folder,
         Executable_Path   => Bin_Folder,
         Alix_Config_Path  => Alix_Config);

      Alix.Processes.Spawn ("gprbuild alix_setup.gpr");

      Ada.Directories.Copy_File
        (Source_Name => Executable_Name ("bin/alix-driver"),
         Target_Name =>
           Ada.Directories.Compose
             (Bin_Folder, Executable_Name ("alix")));

      GNAT.OS_Lib.Set_Executable
         (Ada.Directories.Compose
             (Bin_Folder, Executable_Name ("alix")));

      Ada.Text_IO.Put_Line
        ("Setup complete");
      Ada.Text_IO.Put_Line
        ("Ensure that " & Bin_Folder & " is in your PATH");
      Ada.Text_IO.Put_Line
        ("Ensure that " & Alix_Folder & " is in GPR_PROJECT_PATH");

   end;

end Setup_Driver;
