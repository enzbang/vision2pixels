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

with Ada.Text_IO;
with Ada.Exceptions;

with Settings;

with Wiki_Interface;
with Gwiad.Plugins.Services.Cache;
with Gwiad.Plugins.Services.Registry;

package body V2P.Wiki is

   use Ada;
   use Gwiad.Plugins.Services.Cache;
   use Gwiad.Plugins.Services.Registry;

   Wiki_Id : Service_Id;
   Has_Service : Boolean := False;

   function Get return Wiki_Interface.GW_Service'Class;
   --  Returns the service

   function Wiki_Service_Name return Service_Name;
   --  Returns the service name that provide wiki service

   ---------
   -- Get --
   ---------

   function Get return Wiki_Interface.GW_Service'Class is
      use Wiki_Interface;
   begin
      if not Has_Service then
         Initialize_Wiki : declare
            Wiki_World_Service_Access : constant not null GW_Service_Access
              := GW_Service_Access (Get (Wiki_Service_Name));
            Get_Service               : constant GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            Initialize
              (S              =>
                 GW_Service'Class (Wiki_World_Service_Access.all),
               Base_URL       => "/",
               Img_Base_URL   => Settings.Images_Source_Prefix,
               Text_Directory => "");

            Wiki_Id := Set (Wiki_Service_Name,
                            Gwiad.Plugins.Services.Service_Access
                              (Wiki_World_Service_Access));

            Has_Service := True;
            return Get_Service;
         end Initialize_Wiki;

      else
         Return_Service : declare
            Wiki_World_Service_Access : constant not null GW_Service_Access :=
                                          GW_Service_Access (Get (Wiki_Id));
            Get_Service               : constant GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            return Get_Service;
         end Return_Service;
      end if;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         raise;
   end Get;

   -----------------------
   -- Wiki_Service_Name --
   -----------------------

   function Wiki_Service_Name return Service_Name is
   begin
      return Service_Name (Settings.Wiki_Service_Name);
   end Wiki_Service_Name;

   ------------------
   -- Wiki_To_HTML --
   ------------------

   function Wiki_To_HTML (S : in String) return String is
   begin
      if not Exists (Wiki_Service_Name) then
         Text_IO.Put_Line ("Does not exists;");
         return "Doest not exists";
      end if;

      Render : declare
         Get_Service : constant Wiki_Interface.GW_Service'Class := Get;
      begin
         return Wiki_Interface.HTML_Preview (Get_Service, S);
      end Render;
   end Wiki_To_HTML;


end V2P.Wiki;
