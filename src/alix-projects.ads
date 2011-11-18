with Ada.Containers.Indefinite_Vectors;

package Alix.Projects is

   type Alix_Project is private;

   function Create_Project
     (Project_Name   : String;
      Trunk_Name     : String)
     return Alix_Project;

   procedure Add_Source_Directory
     (Project : in out Alix_Project;
      Path    : in     String);

   procedure Enable_Style_Checks
     (Project : in out Alix_Project;
      Enabled : in     Boolean);

   procedure Enable_Ada_2012
     (Project : in out Alix_Project;
      Enabled : in     Boolean);

private

   package String_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Alix_Project is
      record
         Project_Name : access String;
         Trunk_Name   : access String;
         Source_Dirs  : String_Vectors.Vector;
         Style_Checks : Boolean                 := True;
         Ada_2012     : Boolean                 := False;
      end record;

end Alix.Projects;
