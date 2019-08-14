package Alix.Projects is

   Project_Not_Found : exception;

   function Fetch_Project
     (Project_Name     : String;
      Version_Template : String)
      return String;
   --  Find a project with the given name which matches the version template
   --  If the version template is "*" or "any", get the most recent version
   --  If the project source is not already stored locally, fetch it from
   --  the server named in the Project.alix config file
   --  Return the full path to the project's source folder
   --  The Project_Not_Found exception is raised if the project does not
   --  exist, or the version template cannot be matched.

   function Get_Matching_Version
     (Project_Name     : String;
      Version_Template : String)
      return String;

end Alix.Projects;
