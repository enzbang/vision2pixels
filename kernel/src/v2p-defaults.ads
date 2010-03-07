------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2010                          --
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

with Morzhol.Logs;

with V2P.Settings;

package V2P.Defaults is

   DB                          : V2P.Settings.DB_Kind := V2P.Settings.SQLite3;

   DB_Name                     : constant String := "v2p.db";

   Images_Path                 : constant String := "images";

   Medium_Path                 : constant String := "images_medium";

   Thumbs_Path                 : constant String := "thumbs";

   Website_Data_Path           : constant String := "web_data";

   Big_Images_Source_Prefix    : constant String := "/photos";

   Medium_Images_Source_Prefix : constant String := "/photos-m";

   Thumbs_Source_Prefix        : constant String := "/thumbs";

   Website_Data_Prefix         : constant String := "/web";

   Anonymous_Visit_Counter     : constant Boolean := False;

   Anonymous_Comment           : constant Boolean := False;

   Anonymity_Hours             : constant Natural := 3 * 24;

   Posting_Delay_Hours         : constant Natural := 3 * 24;

   Ignore_Author_Click         : constant Boolean := True;

   Limit_Image_Size            : constant Boolean := True;

   Image_Maximum_Height        : constant Positive := 1000;

   Image_Maximum_Width         : constant Positive := 1000;

   Image_Maximum_Size          : constant Integer := 1_000_000;

   Thumbnail_Maximum_Width     : constant Positive := 150;

   Thumbnail_Maximum_Height    : constant Positive := 150;

   Medium_Maximum_Width        : constant Positive := 800;

   Medium_Maximum_Height       : constant Positive := 800;

   Descending_Order            : constant Boolean := True;

   Wiki_Service_Name           : constant String := "wiki_service";

   Virtual_Host                : constant String := "127.0.0.10";

   RSS_Host_URL                : constant String := "http://127.0.0.10:8080";

   Number_Latest_Posts         : constant Positive := 6;

   Number_Latest_Users         : constant Positive := 5;

   Number_Latest_User_Posts    : constant Positive := 24;
   --  24 is good as it is a multiple of 3 and 4 so will layout properly on 3
   --  or 4 columns.

   Number_Latest_User_Messages : constant Positive := 100;

   Google_Map_Key              : constant String := "";

   Log_Path                    : constant String := "./";

   Cache_Path                  : constant String := "./";

   RSS_Path                    : constant String := "./";

   RSS_Prefix                  : constant String := "/rss";

   Compression                 : constant Boolean := False;

   Max_Search_Results          : constant Positive := 100;

   Number_Users_Listed         : constant Positive := 20;

   Number_CdC_Listed           : constant Positive := 20;

   SMTP_Server                 : constant String := "localhost";

   Default_Timezone            : constant String := "+60";

   RSS_Latest_Comments         : constant Positive := 40;

   RSS_Latest_Posts            : constant Positive := 15;

   Log_Level                   : constant Morzhol.Logs.Log_Level :=
                                   Morzhol.Logs.Warnings;

end V2P.Defaults;
