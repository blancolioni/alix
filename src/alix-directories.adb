with Ada.Directories;
with Ada.Strings.Unbounded;

package body Alix.Directories is

   -------------------------
   -- Compose_Directories --
   -------------------------

   function Compose_Directories
     (Containing_Directory : String;
      Sub_Directory        : String)
      return String
   is
   begin
      if Sub_Directory'Length = 0 then
         return Containing_Directory;
      end if;

      for I in Sub_Directory'Range loop
         if Sub_Directory (I) = '/'
           or else Sub_Directory (I) = '\'
         then
            return Compose_Directories
              (Ada.Directories.Compose
                 (Containing_Directory,
                  Sub_Directory (Sub_Directory'First .. I - 1)),
               Sub_Directory (I + 1 .. Sub_Directory'Last));
         end if;
      end loop;

      return Ada.Directories.Compose (Containing_Directory,
                                      Sub_Directory);
   end Compose_Directories;

   --------------------
   -- Copy_Directory --
   --------------------

   procedure Copy_Directory
     (Source_Directory : String;
      Target_Directory : String)
   is
      use Ada.Strings.Unbounded;
      Local_Target : Unbounded_String :=
                       To_Unbounded_String (Target_Directory);

      procedure Copy_File
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Ada.Directories.Copy_File
           (Ada.Directories.Full_Name (Directory_Entry),
            Ada.Directories.Compose
              (To_String (Local_Target),
               Ada.Directories.Simple_Name (Directory_Entry)));
      end Copy_File;

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Name : constant String := Simple_Name (Directory_Entry);
         Saved_Target : constant Unbounded_String := Local_Target;
      begin
         if Name = "." or else Name = ".." then
            return;
         end if;

         Local_Target :=
           To_Unbounded_String
             (Compose
                  (To_String (Local_Target),
                   Simple_Name (Directory_Entry)));

         Ada.Directories.Create_Path (To_String (Local_Target));

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*",
            Filter         => (Ada.Directories.Ordinary_File => True,
                               others                        => False),
            Process        => Copy_File'Access);

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*",
            Filter         => (Ada.Directories.Directory     => True,
                               others                        => False),
            Process        => Recurse'Access);

         Local_Target := Saved_Target;

      end Recurse;

   begin

      Ada.Directories.Create_Path (Target_Directory);

      Ada.Directories.Search
        (Directory      => Source_Directory,
         Pattern        => "*",
         Filter         => (Ada.Directories.Ordinary_File => True,
                            others => False),
         Process        => Copy_File'Access);

      Ada.Directories.Search
        (Directory      => Source_Directory,
         Pattern        => "*",
         Filter         => (Ada.Directories.Directory     => True,
                            others => False),
         Process        => Recurse'Access);

   end Copy_Directory;

end Alix.Directories;
