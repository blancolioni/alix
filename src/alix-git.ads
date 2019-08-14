package Alix.Git is

   procedure Clone
     (Local_Path      : String;
      Repository_Path : String;
      Branch_Name     : String := "");

   procedure Pull;

end Alix.Git;
