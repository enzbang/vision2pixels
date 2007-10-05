------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
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

with V2P.Settings;

package V2P.Defaults is

   DB                       : V2P.Settings.DB_Kind := V2P.Settings.SQLite3;

   DB_Name                  : constant String := "v2p.db";

   Images_Path              : constant String := "images";

   Thumbs_Path              : constant String := "thumbs";

   Website_Data_Path        : constant String := "web_data";

   Images_Source_Prefix     : constant String := "/photos";

   Thumbs_Source_Prefix     : constant String := "/thumbs";

   Website_Data_Prefix      : constant String := "/web";

   Anonymous_Visit_Counter  : constant Boolean := False;

   Anonymous_Comment        : constant Boolean := False;

   Anonymity_Hours          : constant Natural := 3 * 24;

   Ignore_Author_Click      : constant Boolean := True;

   Limit_Image_Size         : constant Boolean := True;

   Image_Maximum_Height     : constant Integer := 800;

   Image_Maximum_Width      : constant Integer := 800;

   Image_Maximum_Size       : constant Integer := 1_000_000;

   Thumbnail_Maximum_Width  : constant Integer := 150;

   Thumbnail_Maximum_Height : constant Integer := 150;

   Descending_Order         : constant Boolean := True;

   Wiki_Service_Name        : constant String  := "wiki_service";

   Virtual_Host             : constant String  := "127.0.0.10";

   Number_Latest_Posts      : constant Positive := 6;

   Number_Latest_Users      : constant Positive := 5;

end V2P.Defaults;
