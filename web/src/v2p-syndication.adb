------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2008                            --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

with AWS.Templates;

with Morzhol.OS;

with V2P.Database;
with V2P.Settings;
with V2P.Template_Defs.Page_Rss_Recent_Photos;
with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Set_Global;

package body V2P.Syndication is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   ----------------------------
   -- Update_RSS_Last_Photos --
   ----------------------------

   procedure Update_RSS_Last_Photos (Create_Only : in Boolean := False) is
      Filename     : constant String := Morzhol.OS.Compose
        (Settings.RSS_Path,
         Directories.Simple_Name
           (Template_Defs.Page_Rss_Recent_Photos.Set.URL));
      Translations : Templates.Translate_Set;
      File         : File_Type;

   begin

      if Create_Only and then Directories.Exists (Filename) then

         --  Nothing to do. Quit
         return;
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.URL));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.THUMB_SOURCE_PREFIX,
            Settings.Thumbs_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Page_Rss_Recent_Photos.V2P_URL,
            V2P.Settings.RSS_Host_URL));

      Templates.Insert
        (Translations,
         Database.Get_Latest_Posts
           (Limit    => 15,
            Add_Date => True));

      Create (File => File,
              Mode => Out_File,
              Name => Filename);

      Put (File => File,
           Item => Templates.Parse
             (Filename     => Template_Defs.Page_Rss_Recent_Photos.Template,
              Translations  => Translations));

      Close (File);
   end Update_RSS_Last_Photos;

end V2P.Syndication;
