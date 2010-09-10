------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2008                             --
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

with Morzhol.OS;
with V2P.Settings;
with V2P.Template_Defs.Page_User;

package body V2P.URL is

   use V2P;

   ------------------------
   -- Avatar_Full_Prefix --
   ------------------------

   function Avatar_Full_Prefix return String is
   begin
      return Morzhol.OS.Compose
        (Gwiad_Plugin_Path, Settings.Get_Avatars_Path);
   end Avatar_Full_Prefix;

   ----------------------------
   -- Big_Images_Full_Prefix --
   ----------------------------

   function Big_Images_Full_Prefix return String is
   begin
      return Morzhol.OS.Compose
        (Gwiad_Plugin_Path, Settings.Get_Big_Images_Path);
   end Big_Images_Full_Prefix;

   -------------------------------
   -- Medium_Images_Full_Prefix --
   -------------------------------

   function Medium_Images_Full_Prefix return String is
   begin
      return Morzhol.OS.Compose
        (Gwiad_Plugin_Path, Settings.Get_Medium_Images_Path);
   end Medium_Images_Full_Prefix;

   ------------------------
   -- Thumbs_Full_Prefix --
   ------------------------

   function Thumbs_Full_Prefix return String is
   begin
      return Morzhol.OS.Compose (Gwiad_Plugin_Path, Settings.Get_Thumbs_Path);
   end Thumbs_Full_Prefix;

   ---------------
   -- User_Name --
   ---------------

   function User_Name (URL : in String) return String is
   begin
      return URL
        (URL'First + Template_Defs.Page_User.Set.URL'Length .. URL'Last);
   end User_Name;

end V2P.URL;
