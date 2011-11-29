with Alix.Versions;

package Alix.Status is

   function Is_Installed (Project_Name : String;
                          Version      : Alix.Versions.Version_Number)
                          return Boolean;

--     function Get_Matching_Version
--       (Project_Name     : String;
--        Version_Template : Alix.Versions.Version_Number)
--        return Alix.Versions.Version_Number;

   function Get_GPR_Project_Name
     (Project_Name : String;
      Version      : Alix.Versions.Version_Number)
      return String;

   function GPR_Project_Path
     (Project_Name    : String;
      Project_Version : String)
      return String;

   function Latest_Installed_Version
     (Project_Name : String)
      return Alix.Versions.Version_Number;

end Alix.Status;
