package Alix.Versions is

   subtype Version_Number is String;

   function Match_Versions
     (Version : Version_Number;
      Template : Version_Number)
      return Boolean;

end Alix.Versions;
