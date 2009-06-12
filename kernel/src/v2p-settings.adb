------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2009                          --
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

with Ada.Exceptions;
with Ada.Text_IO;

with V2P.Defaults;
with Morzhol.Iniparser;

package body V2P.Settings is

   use Ada;
   use Ada.Exceptions;

   Config_Filename : constant String := "plugins/vision2pixels/v2p.ini";

   type Attributes is
     (DB, DB_Name, Images_Path, Thumbs_Path, Medium_Image_Path,
      Images_Source_Prefix, Thumbs_Source_Prefix, Medium_Images_Source_Prefix,
      Anonymous_Visit_Counter, Anonymous_Comment, Anonymity_Hours,
      Posting_Delay_Hours, Descending_Order, Ignore_Author_Click,
      Limit_Image_Size, Image_Maximum_Width, Image_Maximum_Height,
      Image_Maximum_Size, Medium_Maximum_Width, Medium_Maximum_Height,
      Thumbnail_Maximum_Width, Thumbnail_Maximum_Height, Virtual_Host,
      Website_Data_Path, Number_Latest_User_Posts, Number_Latest_User_Messages,
      Website_Data_Prefix, Wiki_Service_Name, Number_Latest_Posts,
      Number_Latest_Users, Google_Map_Key, Log_Path, Cache_Path, RSS_Host_URL,
      RSS_Path, RSS_Prefix, Compression, Max_Search_Results, SMTP_Server,
      Number_Users_Listed, Default_Timezone);

   package Conf is new Morzhol.Iniparser (Parameter_Name => Attributes);

   package DB_Conf is new Conf.Enum_Values (Enum => DB_Kind);

   ---------------------
   -- Anonymity_Hours --
   ---------------------

   function Anonymity_Hours return Natural is
   begin
      return Conf.Get_Value (Anonymity_Hours);
   end Anonymity_Hours;

   -----------------------
   -- Anonymous_Comment --
   -----------------------

   function Anonymous_Comment return Boolean is
   begin
      return Conf.Get_Value (Anonymous_Comment);
   end Anonymous_Comment;

   -----------------------------
   -- Anonymous_Visit_Counter --
   -----------------------------

   function Anonymous_Visit_Counter return Boolean is
   begin
      return Conf.Get_Value (Anonymous_Visit_Counter);
   end Anonymous_Visit_Counter;

   ------------------------------
   -- Big_Images_Source_Prefix --
   ------------------------------

   function Big_Images_Source_Prefix return String is
   begin
      return Conf.Get_Value (Images_Source_Prefix);
   end Big_Images_Source_Prefix;

   ----------------
   -- Cache_Path --
   ----------------

   function Cache_Path return String is
   begin
      return Conf.Get_Value (Cache_Path);
   end Cache_Path;

   -----------------
   -- Compression --
   -----------------

   function Compression return Boolean is
   begin
      return Conf.Get_Value (Compression);
   end Compression;

   ----------------------
   -- Default_Timezone --
   ----------------------

   function Default_Timezone return String is
   begin
      return Conf.Get_Value (Default_Timezone);
   end Default_Timezone;

   ----------------------
   -- Descending_Order --
   ----------------------

   function Descending_Order return Boolean is
   begin
      return Conf.Get_Value (Descending_Order);
   end Descending_Order;

   -------------------------
   -- Get_Big_Images_Path --
   -------------------------

   function Get_Big_Images_Path return String is
   begin
      return Conf.Get_Value (Images_Path);
   end Get_Big_Images_Path;

   ------------
   -- Get_DB --
   ------------

   function Get_DB return DB_Kind is
   begin
      return DB_Conf.Get_Value (DB);
   end Get_DB;

   -----------------
   -- Get_DB_Name --
   -----------------

   function Get_DB_Name return String is
   begin
      return Conf.Get_Value (DB_Name);
   end Get_DB_Name;

   ----------------------------
   -- Get_Medium_Images_Path --
   ----------------------------

   function Get_Medium_Images_Path return String is
   begin
      return Conf.Get_Value (Medium_Image_Path);
   end Get_Medium_Images_Path;

   ---------------------
   -- Get_Thumbs_Path --
   ---------------------

   function Get_Thumbs_Path return String is
   begin
      return Conf.Get_Value (Thumbs_Path);
   end Get_Thumbs_Path;

   --------------------
   -- Goggle_Map_Key --
   --------------------

   function Google_Map_Key return String is
   begin
      return Conf.Get_Value (Google_Map_Key);
   end Google_Map_Key;

   -------------------------
   -- Ignore_Author_Click --
   -------------------------

   function Ignore_Author_Click return Boolean is
   begin
      return Conf.Get_Value (Ignore_Author_Click);
   end Ignore_Author_Click;

   --------------------------
   -- Image_Maximum_Height --
   --------------------------

   function Image_Maximum_Height return Natural is
   begin
      return Conf.Get_Value (Image_Maximum_Height);
   end Image_Maximum_Height;

   ------------------------
   -- Image_Maximum_Size --
   ------------------------

   function Image_Maximum_Size return Natural is
   begin
      return Conf.Get_Value (Image_Maximum_Size);
   end Image_Maximum_Size;

   -------------------------
   -- Image_Maximum_Width --
   -------------------------

   function Image_Maximum_Width return Natural is
   begin
      return Conf.Get_Value (Image_Maximum_Width);
   end Image_Maximum_Width;

   ----------------------
   -- Limit_Image_Size --
   ----------------------

   function Limit_Image_Size return Boolean is
   begin
      return Conf.Get_Value (Limit_Image_Size);
   end Limit_Image_Size;

   --------------
   -- Log_Path --
   --------------

   function Log_Path return String is
   begin
      return Conf.Get_Value (Log_Path);
   end Log_Path;

   ------------------------
   -- Max_Search_Results --
   ------------------------

   function Max_Search_Results return Positive is
   begin
      return Conf.Get_Value (Max_Search_Results);
   end Max_Search_Results;

   ---------------------------------
   -- Medium_Images_Source_Prefix --
   ---------------------------------

   function Medium_Images_Source_Prefix return String is
   begin
      return Conf.Get_Value (Medium_Images_Source_Prefix);
   end Medium_Images_Source_Prefix;

   --------------------------
   -- Medium_Maximum_Height --
   --------------------------

   function Medium_Maximum_Height return Natural is
   begin
      return Conf.Get_Value (Medium_Maximum_Height);
   end Medium_Maximum_Height;

   --------------------------
   -- Medium_Maximum_Width --
   --------------------------

   function Medium_Maximum_Width return Natural is
   begin
      return Conf.Get_Value (Medium_Maximum_Width);
   end Medium_Maximum_Width;

   -------------------------
   -- Number_Latest_Posts --
   -------------------------

   function Number_Latest_Posts return Positive is
   begin
      return Conf.Get_Value (Number_Latest_Posts);
   end Number_Latest_Posts;

   ---------------------------------
   -- Number_Latest_User_Messages --
   ---------------------------------

   function Number_Latest_User_Messages return Positive is
   begin
      return Conf.Get_Value (Number_Latest_User_Messages);
   end Number_Latest_User_Messages;

   ------------------------------
   -- Number_Latest_User_Posts --
   ------------------------------

   function Number_Latest_User_Posts return Positive is
   begin
      return Conf.Get_Value (Number_Latest_User_Posts);
   end Number_Latest_User_Posts;

   -------------------------
   -- Number_Latest_Users --
   -------------------------

   function Number_Latest_Users return Positive is
   begin
      return Conf.Get_Value (Number_Latest_Users);
   end Number_Latest_Users;

   -------------------------
   -- Number_Users_Listed --
   -------------------------

   function Number_Users_Listed return Positive is
   begin
      return Conf.Get_Value (Number_Users_Listed);
   end Number_Users_Listed;

   -------------------------
   -- Posting_Delay_Hours --
   -------------------------

   function Posting_Delay_Hours return Natural is
   begin
      return Conf.Get_Value (Posting_Delay_Hours);
   end Posting_Delay_Hours;

   ------------------
   -- RSS_Host_URL --
   ------------------

   function RSS_Host_URL return String is
   begin
      return Conf.Get_Value (RSS_Host_URL);
   end RSS_Host_URL;

   --------------
   -- RSS_Path --
   --------------

   function RSS_Path return String is
   begin
      return Conf.Get_Value (RSS_Path);
   end RSS_Path;

   ----------------
   -- RSS_Prefix --
   ----------------

   function RSS_Prefix return String is
   begin
      return Conf.Get_Value (RSS_Prefix);
   end RSS_Prefix;

   -----------------
   -- SMTP_Server --
   -----------------

   function SMTP_Server return String is
   begin
      return Conf.Get_Value (SMTP_Server);
   end SMTP_Server;

   ------------------------------
   -- Thumbnail_Maximum_Height --
   ------------------------------

   function Thumbnail_Maximum_Height return Natural is
   begin
      return Conf.Get_Value (Thumbnail_Maximum_Height);
   end Thumbnail_Maximum_Height;

   -----------------------------
   -- Thumbnail_Maximum_Width --
   -----------------------------

   function Thumbnail_Maximum_Width return Natural is
   begin
      return Conf.Get_Value (Thumbnail_Maximum_Width);
   end Thumbnail_Maximum_Width;

   --------------------------
   -- Thumbs_Source_Prefix --
   --------------------------

   function Thumbs_Source_Prefix return String is
   begin
      return Conf.Get_Value (Thumbs_Source_Prefix);
   end Thumbs_Source_Prefix;

   ------------------
   -- Virtual_Host --
   ------------------

   function Virtual_Host return String is
   begin
      return Conf.Get_Value (Virtual_Host);
   end Virtual_Host;

   -----------------------
   -- Website_Data_Path --
   -----------------------

   function Website_Data_Path return String is
   begin
      return Conf.Get_Value (Website_Data_Path);
   end Website_Data_Path;

   -------------------------
   -- Website_Data_Prefix --
   -------------------------

   function Website_Data_Prefix return String is
   begin
      return Conf.Get_Value (Website_Data_Prefix);
   end Website_Data_Prefix;

   -----------------------
   -- Wiki_Service_Name --
   -----------------------

   function Wiki_Service_Name return String is
   begin
      return Conf.Get_Value (Wiki_Service_Name);
   end Wiki_Service_Name;

begin --  V2P.Settings : Set default values

   DB_Conf.Set_Value (DB, Defaults.DB);
   Conf.Set_Value (DB_Name, Defaults.DB_Name);
   Conf.Set_Value (Images_Path, Defaults.Images_Path);
   Conf.Set_Value (Medium_Image_Path, Defaults.Medium_Path);
   Conf.Set_Value (Thumbs_Path, Defaults.Thumbs_Path);
   Conf.Set_Value (Images_Source_Prefix, Defaults.Big_Images_Source_Prefix);
   Conf.Set_Value (Medium_Images_Source_Prefix,
                   Defaults.Medium_Images_Source_Prefix);
   Conf.Set_Value (Thumbs_Source_Prefix, Defaults.Thumbs_Source_Prefix);
   Conf.Set_Value (Anonymous_Visit_Counter, Defaults.Anonymous_Visit_Counter);
   Conf.Set_Value (Anonymous_Comment, Defaults.Anonymous_Comment);
   Conf.Set_Value (Anonymity_Hours, Defaults.Anonymity_Hours);
   Conf.Set_Value (Posting_Delay_Hours, Defaults.Posting_Delay_Hours);
   Conf.Set_Value (Descending_Order, Defaults.Descending_Order);
   Conf.Set_Value (Ignore_Author_Click, Defaults.Ignore_Author_Click);
   Conf.Set_Value (Limit_Image_Size, Defaults.Limit_Image_Size);
   Conf.Set_Value (Image_Maximum_Height, Defaults.Image_Maximum_Height);
   Conf.Set_Value (Image_Maximum_Width, Defaults.Image_Maximum_Width);
   Conf.Set_Value (Medium_Maximum_Width, Defaults.Medium_Maximum_Width);
   Conf.Set_Value (Medium_Maximum_Height, Defaults.Medium_Maximum_Height);
   Conf.Set_Value (Image_Maximum_Size, Defaults.Image_Maximum_Size);
   Conf.Set_Value (Thumbnail_Maximum_Width, Defaults.Thumbnail_Maximum_Width);
   Conf.Set_Value (Thumbnail_Maximum_Height,
                   Defaults.Thumbnail_Maximum_Height);
   Conf.Set_Value (Virtual_Host, Defaults.Virtual_Host);
   Conf.Set_Value (Website_Data_Path, Defaults.Website_Data_Path);
   Conf.Set_Value (Website_Data_Prefix, Defaults.Website_Data_Prefix);
   Conf.Set_Value (Wiki_Service_Name, Defaults.Wiki_Service_Name);
   Conf.Set_Value (Number_Latest_Posts, Defaults.Number_Latest_Posts);
   Conf.Set_Value (Number_Latest_Users, Defaults.Number_Latest_Users);
   Conf.Set_Value (Number_Latest_User_Posts,
                   Defaults.Number_Latest_User_Posts);
   Conf.Set_Value (Number_Latest_User_Messages,
                   Defaults.Number_Latest_User_Messages);
   Conf.Set_Value (Google_Map_Key, Defaults.Google_Map_Key);
   Conf.Set_Value (Log_Path, Defaults.Log_Path);
   Conf.Set_Value (Cache_Path, Defaults.Cache_Path);
   Conf.Set_Value (RSS_Host_URL, Defaults.RSS_Host_URL);
   Conf.Set_Value (RSS_Path, Defaults.RSS_Path);
   Conf.Set_Value (RSS_Prefix, Defaults.RSS_Prefix);
   Conf.Set_Value (Compression, Defaults.Compression);
   Conf.Set_Value (Max_Search_Results, Defaults.Max_Search_Results);
   Conf.Set_Value (Number_Users_Listed, Defaults.Number_Users_Listed);
   Conf.Set_Value (SMTP_Server, Defaults.SMTP_Server);
   Conf.Set_Value (Default_Timezone, Defaults.Default_Timezone);

   --  Now read the config file if any

   Conf.IO.Open (Config_Filename);
   Conf.IO.Close;

exception
   when Conf.IO.Uncomplete_Config =>
      Conf.IO.Close;
   when UP : Conf.IO.Unknown_Parameter =>
      Text_IO.Put_Line (Exception_Message (UP));
      Conf.IO.Close;
   when Text_IO.Name_Error =>
      Text_IO.Put_Line ("Config file '" & Config_Filename & "' not found.");
      null;
end V2P.Settings;
