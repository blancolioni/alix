with Alix.Processes;

package body Alix.Git is

   -----------
   -- Clone --
   -----------

   procedure Clone
     (Local_Path      : String;
      Repository_Path : String;
      Branch_Name     : String := "")
   is
   begin
      Alix.Processes.Spawn
        ("git clone "
         & (if Branch_Name /= "" then "--branch " & Branch_Name & " "
           else "")
         & Repository_Path
         & " "
         & Local_Path);
   end Clone;

   ----------
   -- Pull --
   ----------

   procedure Pull is
   begin
      Alix.Processes.Spawn ("git pull");
   end Pull;

end Alix.Git;
