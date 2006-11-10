------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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

with Config;

package body Settings is

   Config_Filename : constant String := "v2p.ini";

   type Attributes is
     (DB, DB_Name, Images_Path, Thumbs_Path,
      Anonymous_Visit_Counter, Ignore_Author_Click);

   package Conf is new Config (Attributes);

   package DB_Conf is new Conf.Enum_Values (DB_Kind);

   -----------------------------
   -- Anonymous_Visit_Counter --
   -----------------------------

   function Anonymous_Visit_Counter return Boolean is
   begin
      return Conf.Get_Value (Anonymous_Visit_Counter);
   end Anonymous_Visit_Counter;

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

begin
   Conf.IO.Open (Config_Filename);
exception
   when others =>
      null;
end Settings;
