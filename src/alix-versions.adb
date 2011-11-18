with Ada.Characters.Handling;

package body Alix.Versions is

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
