with Alix.Commands;
with Alix.Config;

procedure Alix.Driver is

begin

   Alix.Config.Read_Config;
   Alix.Commands.Initialise;
   Alix.Commands.Execute;

end Alix.Driver;
