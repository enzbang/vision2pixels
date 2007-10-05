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

package V2P.Settings is

   type DB_Kind is (SQLite3, ODBC);

   function Get_DB return DB_Kind;
   --  Returns the DB kind to use

   function Get_DB_Name return String;
   --  The database name, either the ODBC name or the path to the SQLite
   --  filename.

   function Get_Images_Path return String;
   --  Returns the path to the images

   function Get_Thumbs_Path return String;
   --  Returns the path to the thumbnails

   function Images_Source_Prefix return String;
   --  Source prefix used to reference images in URL

   function Thumbs_Source_Prefix return String;
   --  Source prefix used to reference thumbs in URL

   function Anonymous_Visit_Counter return Boolean;
   --  Counts anonymous clicks or only logged user clicks

   function Ignore_Author_Click return Boolean;
   --  If not Anonymous_Visit_Counter, ignore author click on his photos

   function Anonymous_Comment return Boolean;
   --  If true, allows anonymous comment

   function Anonymity_Hours return Natural;
   --  Number of hours that a post stay anonymous

   function Limit_Image_Size return Boolean;
   --  If set, check image size on creation

   function Image_Maximum_Width return Natural;
   --  Maximum width dimension of an uploaded image

   function Image_Maximum_Height return Natural;
   --  Maximum height dimension of an uploaded image

   function Image_Maximum_Size return Natural;
   --  Limit, in bytes, of an uploaded image size

   function Thumbnail_Maximum_Width return Integer;
   --  Maximum width dimension of a thumbnail image

   function Thumbnail_Maximum_Height return Integer;
   --  Maximum height dimension of a thumbnail image

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

end V2P.Settings;
