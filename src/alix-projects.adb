package body Alix.Projects is

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory
     (Project : in out Alix_Project;
      Path    : in     String)
   is
   begin
      Project.Source_Dirs.Append (Path);
   end Add_Source_Directory;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Project_Name   : String;
      Trunk_Name     : String)
     return Alix_Project
   is
   begin
      return Result : Alix_Project do
        Result.Project_Name := new String'(Project_Name);
        Result.Trunk_Name   := new String'(Trunk_Name);
      end return;
   end Create_Project;

   -------------------------
   -- Enable_Style_Checks --
   -------------------------

   procedure Enable_Style_Checks
     (Project : in out Alix_Project;
      Enabled : in     Boolean)
   is
   begin
      Project.Style_Checks := Enabled;
   end Enable_Style_Checks;

   ---------------------
   -- Enable_Ada_2012 --
   ---------------------

   procedure Enable_Ada_2012
     (Project : in out Alix_Project;
      Enabled : in     Boolean)
   is
   begin
      Project.Ada_2012 := Enabled;
   end Enable_Ada_2012;

end Alix.Projects;
