package Alix.Installer is

   procedure  Install (Project_Name    : String;
                       Project_Version : String := "Trunk");

   procedure  Configure (Directory : String);

end Alix.Installer;
