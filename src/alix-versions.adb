with Ada.Characters.Handling;

package body Alix.Versions is

   ----------------------
   -- Get_Project_Name --
   ----------------------

   function Get_Project_Name
     (Project_And_Version : String)
      return String
   is
   begin
      for I in Project_And_Version'Range loop
         if Project_And_Version (I) = '-' then
            return Project_And_Version
              (Project_And_Version'First .. I - 1);
         end if;
      end loop;
      return Project_And_Version;
   end Get_Project_Name;

   -------------------------
   -- Get_Project_Version --
   -------------------------

   function Get_Project_Version
     (Project_And_Version : String)
      return Version_Number
   is
   begin
      for I in Project_And_Version'Range loop
         if Project_And_Version (I) = '-' then
            return Project_And_Version
              (I + 1 .. Project_And_Version'Last);
         end if;
      end loop;
      return "*";
   end Get_Project_Version;

   --------------------
   -- Match_Versions --
   --------------------

   function Match_Versions
     (Version : Version_Number;
      Template : Version_Number)
      return Boolean
   is
      use Ada.Characters.Handling;
   begin
      if Template = "any" then
         return True;
      else
         declare
            V : Positive := Version'First;
         begin
            for T in Template'Range loop
               if V > Version'Last then
                  return False;
               end if;

               if Template (T) = '*' then
                  if Is_Digit (Version (V)) then
                     while V <= Version'Last
                       and then Is_Digit (Version (V))
                     loop
                        V := V + 1;
                     end loop;
                  else
                     return False;
                  end if;
               elsif Template (T) /= Version (V) then
                  return False;
               else
                  V := V + 1;
               end if;
            end loop;
            return True;
         end;
      end if;
   end Match_Versions;

end Alix.Versions;
