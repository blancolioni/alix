package Alix.Directories is

   procedure Copy_Directory
     (Source_Directory : String;
      Target_Directory : String);

   function Compose_Directories
     (Containing_Directory : String;
      Sub_Directory        : String)
      return String;

end Alix.Directories;
