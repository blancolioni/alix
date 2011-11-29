with WL.Text;

package Alix.Config is

   procedure Read_Config;

   function Temporary_File_Path
     (File_Name : String)
     return String;

   function Installation_Path
     (Project_Name    : String;
      Version_Name    : String)
      return String;

   function Project_Config_Path
     (Project_Name    : String;
      Project_Version : String)
      return String;

   function Configuration_File_Path
     (File_Name : String)
     return String;

   function Global_Build_Path return String;
   function Global_Exec_Path return String;
   function Global_Object_Path return String;
   function Global_Source_Path return String;

   function Pull_Script_Template_Path
     return String;

   function Server_URL (Project_Name : String;
                        Version_Name : String)
                       return String;

   function Get (Key : String) return String;

   function Standard_Replacement
     (Project_Name    : String;
      Project_Version : String)
     return WL.Text.Text_Replacement;

end Alix.Config;
