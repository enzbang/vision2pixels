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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with Config;

package body Image.Config is

   Config_File : constant String := "v2p-image.ini";

   Configured : Boolean := False;

   type Config_Parameters is (Images_Path, Thumbs_Path);

   package Image_Config is new Standard.Config (Config_Parameters);

   -------------------
   --  Images_Path  --
   -------------------

   function Images_Path return String is
   begin
      if Configured then
         return Image_Config.Get_Value (Images_Path);
      else
         raise Constraint_Error
           with "(Images_Path) : Image module not configured";
      end if;
   end Images_Path;

   -------------------
   --  Thumbs_Path  --
   -------------------

   function Thumbs_Path return String is
   begin
      if Configured then
         return Image_Config.Get_Value (Thumbs_Path);
      else
         raise Constraint_Error
           with "(Thumbs_Path) : Image module not configured";
      end if;
   end Thumbs_Path;

begin
   Image_Config.IO.Open (Config_File);
   Configured := True;
exception
   when Image_Config.IO.Uncomplete_Config =>
      Configured := True;
   when others =>
      null;
end Image.Config;
