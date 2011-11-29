package Alix.Installer is

   procedure  Install (Project_Name     : String;
                       Version_Template : String := "*");

   procedure  Configure (Directory : String);

end Alix.Installer;
