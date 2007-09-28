------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2007                             --
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
   -- Images_Full_Prefix --
   ------------------------

   function Images_Full_Prefix return String is
   begin
      return Gwiad_Plugin_Path
        & Morzhol.OS.Directory_Separator & Settings.Get_Images_Path;
   end Images_Full_Prefix;

   ---------------
   -- User_Name --
   ---------------

   function User_Name (URL : in String) return String is
   begin
      return URL (URL'First + Template_Defs.Page_User.URL'Length .. URL'Last);
   end User_Name;

end V2P.URL;
