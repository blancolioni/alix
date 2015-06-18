package Alix.Installer is

   procedure  Install (Project_Name     : String;
                       Version_Template : String := "*");

   procedure  Configure
     (Directory : String;
      Mode      : String);

end Alix.Installer;
