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

with Ada.Exceptions;
with Ada.Text_IO;

with V2P.Defaults;
with Morzhol.Iniparser;

package body V2P.Settings is

   use Ada;
   use Ada.Exceptions;

   Config_Filename : constant String := "plugins/vision2pixels/v2p.ini";

   type Attributes is
     (DB, DB_Name, Images_Path, Thumbs_Path, Images_Source_Prefix,
      Thumbs_Source_Prefix, Anonymous_Visit_Counter, Anonymous_Comment,
      Anonymity_Hours, Descending_Order, Ignore_Author_Click, Limit_Image_Size,
      Image_Maximum_Width, Image_Maximum_Height, Image_Maximum_Size,
      Thumbnail_Maximum_Width, Thumbnail_Maximum_Height, Virtual_Host,
      Website_Data_Path, Website_Data_Prefix, Wiki_Service_Name,
      Number_Latest_Posts, Number_Latest_Users);

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

   ----------------------
   -- Descending_Order --
   ----------------------

   function Descending_Order return Boolean is
   begin
      return Conf.Get_Value (Descending_Order);
   end Descending_Order;

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

   ---------------------
   -- Get_Images_Path --
   ---------------------

   function Get_Images_Path return String is
   begin
      return Conf.Get_Value (Images_Path);
   end Get_Images_Path;

   ---------------------
   -- Get_Thumbs_Path --
   ---------------------

   function Get_Thumbs_Path return String is
   begin
      return Conf.Get_Value (Thumbs_Path);
   end Get_Thumbs_Path;

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

   -------------------------
   -- Image_Source_Prefix --
   -------------------------

   function Images_Source_Prefix return String is
   begin
      return Conf.Get_Value (Images_Source_Prefix);
   end Images_Source_Prefix;

   ----------------------
   -- Limit_Image_Size --
   ----------------------

   function Limit_Image_Size return Boolean is
   begin
      return Conf.Get_Value (Limit_Image_Size);
   end Limit_Image_Size;

   -------------------------
   -- Number_Latest_Posts --
   -------------------------

   function Number_Latest_Posts return Positive is
   begin
      return Conf.Get_Value (Number_Latest_Posts);
   end Number_Latest_Posts;

   -------------------------
   -- Number_Latest_Users --
   -------------------------

   function Number_Latest_Users return Positive is
   begin
      return Conf.Get_Value (Number_Latest_Users);
   end Number_Latest_Users;

   ------------------------------
   -- Thumbnail_Maximum_Height --
   ------------------------------

   function Thumbnail_Maximum_Height return Integer is
   begin
      return Conf.Get_Value (Thumbnail_Maximum_Height);
   end Thumbnail_Maximum_Height;

   -----------------------------
   -- Thumbnail_Maximum_Width --
   -----------------------------

   function Thumbnail_Maximum_Width return Integer is
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
   Conf.Set_Value (Thumbs_Path, Defaults.Thumbs_Path);
   Conf.Set_Value (Images_Source_Prefix, Defaults.Images_Source_Prefix);
   Conf.Set_Value (Thumbs_Source_Prefix, Defaults.Thumbs_Source_Prefix);
   Conf.Set_Value (Anonymous_Visit_Counter, Defaults.Anonymous_Visit_Counter);
   Conf.Set_Value (Anonymous_Comment, Defaults.Anonymous_Comment);
   Conf.Set_Value (Anonymity_Hours, Defaults.Anonymity_Hours);
   Conf.Set_Value (Descending_Order, Defaults.Descending_Order);
   Conf.Set_Value (Ignore_Author_Click, Defaults.Ignore_Author_Click);
   Conf.Set_Value (Limit_Image_Size, Defaults.Limit_Image_Size);
   Conf.Set_Value (Image_Maximum_Height, Defaults.Image_Maximum_Height);
   Conf.Set_Value (Image_Maximum_Width, Defaults.Image_Maximum_Width);
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
      null;
end V2P.Settings;
