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

--  This package provided API to retreive embedded metadata in images. These
--  are also known as EXIF, IPTC, XMP metadata.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Image.Metadata.Embedded is

   type Data is record
      Make                : Unbounded_String;
      Camera_Model_Name   : Unbounded_String;
      Shutter_Speed_Value : Unbounded_String;
      Aperture_Value      : Unbounded_String;
      Exposure_Program    : Unbounded_String;
      ISO                 : Unbounded_String;
      Create_Date         : Unbounded_String;
      Metering_Mode       : Unbounded_String;
      Flash               : Unbounded_String;
      Focal_Length        : Unbounded_String;
      Exposure_Mode       : Unbounded_String;
      White_Balance       : Unbounded_String;
   end record;

   No_Data : constant Data;

   function Get (Filename : in String) return Data;
   --  Returns the metadata as read from filename

private

   No_Data : constant Data := (others => Null_Unbounded_String);

end Image.Metadata.Embedded;
