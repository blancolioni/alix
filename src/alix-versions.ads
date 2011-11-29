package Alix.Versions is

   subtype Version_Number is String;

   function Match_Versions
     (Version : Version_Number;
      Template : Version_Number)
      return Boolean;

   function Get_Project_Name
     (Project_And_Version : String)
      return String;
   --  If Project_And_Version is of the form "project-vvv",
   --  return "project".  Otherwise, Project_And_Version is
   --  returned unaltered.

   function Get_Project_Version
     (Project_And_Version : String)
      return Version_Number;
   --  If Project_And_Version is of the form "project-vvv",
   --  return "vvv".  Otherwise, return "*"

end Alix.Versions;
