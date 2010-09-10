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

package V2P.Settings is

   type DB_Kind is (SQLite3, ODBC);

   function Big_Images_Source_Prefix return String;
   --  Source prefix used to reference images in URL

   function Get_Big_Images_Path return String;
   --  Returns the path to the images

   function Get_DB return DB_Kind;
   --  Returns the DB kind to use

   function Get_DB_Name return String;
   --  The database name, either the ODBC name or the path to the SQLite
   --  filename.

   function Get_Thumbs_Path return String;
   --  Returns the path to the thumbnails

   function Get_Medium_Images_Path return String;
   --  Returns the path to the medium size images

   function Get_Avatars_Path return String;
   --  Returns the path to the avatar images

   function Medium_Images_Source_Prefix return String;
   --  Source prefix used to reference medium size images in URL

   function Thumbs_Source_Prefix return String;
   --  Source prefix used to reference thumbs in URL

   function Avatars_Source_Prefix return String;
   --  Source prefix used to reference avatars in URL

   function Anonymous_Visit_Counter return Boolean;
   --  Counts anonymous clicks or only logged user clicks

   function Ignore_Author_Click return Boolean;
   --  If not Anonymous_Visit_Counter, ignore author click on his photos

   function Anonymous_Comment return Boolean;
   --  If true, allows anonymous comment

   function Anonymity_Hours return Natural;
   --  Number of hours that a post stay anonymous

   function Posting_Delay_Hours return Natural;
   --  Number of hours between two posts of the same user

   function Limit_Image_Size return Boolean;
   --  If set, check image size on creation

   function Image_Maximum_Width return Natural;
   --  Maximum width dimension of an uploaded image

   function Image_Maximum_Height return Natural;
   --  Maximum height dimension of an uploaded image

   function Image_Maximum_Size return Natural;
   --  Limit, in bytes, of an uploaded image size

   function Thumbnail_Maximum_Width return Natural;
   --  Maximum width dimension of a thumbnail image

   function Thumbnail_Maximum_Height return Natural;
   --  Maximum height dimension of a thumbnail image

   function Medium_Maximum_Width return Natural;
   --  Maximum width dimension of a medium size image

   function Medium_Maximum_Height return Natural;
   --  Maximum height dimension of a medium size image

   function Avatar_Maximum_Size return Natural;
   --  Maximum height/width dimension of an avatar image

   function Descending_Order return Boolean;
   --  Returns true is the threads view must be sorted in descending order

   function Virtual_Host return String;
   --  Returns v2p virtual host

   function Website_Data_Path return String;
   --  Returns website data path

   function Website_Data_Prefix return String;
   --  Returns website data prefix

   function Wiki_Service_Name return String;
   --  Gwiad wiki service plugin name

   function Number_Latest_Posts return Positive;
   --  Returns the number of latest posts to display for the latest post lazy
   --  tag.

   function Number_Latest_Users return Positive;
   --  Returns the number of latest registered users to display for the latest
   --  users lazy tag.

   function Number_Latest_User_Posts return Positive;
   --  Returns the number of latest posts to display on user's page

   function Number_Latest_User_Messages return Positive;
   --  Returns the number of latest messages to display on user's page

   function Number_CdC_Listed return Positive;
   --  Returns the maximum number of CdC listed on the CdC page

   function Number_Users_Listed return Positive;
   --  Returns the maximum number of users to list in the users page

   function Google_Map_Key return String;
   --  Returns the Google map key

   function Log_Path return String;
   --  Returns the path to use for the log file

   function Cache_Path return String;
   --  Path for the cached files

   function RSS_Host_URL return String;
   --  RSS host url http://hostname:port

   function RSS_Path return String;
   --  Path for the RSS files

   function RSS_Prefix return String;
   --  Returns RSS prefix

   function RSS_Latest_Posts return Positive;
   --  Returns the number of latest posts to retrun for RSS files

   function RSS_Latest_Comments return Positive;
   --  Returns the number of latest comments to retrun for RSS files

   function Compression return Boolean;
   --  Returns True if the HTTP content compression can be used

   function Max_Search_Results return Positive;
   --  Returns the maximum number of match to return in the search page

   function SMTP_Server return String;
   --  Returns the SMTP server name to use for sending e-mails

   function SMTP_Port return Positive;
   --  Returns the SMTP port to use for sending e-mails

   function Default_Timezone return String;
   --  Returns the default time zone offset to use

   function Log_Level return Morzhol.Logs.Log_Level;
   --  Returns the current log level

end V2P.Settings;
