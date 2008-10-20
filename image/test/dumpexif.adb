------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Image.Metadata.Embedded;

procedure DumpExif is
   use Ada;
   use Ada.Strings.Unbounded;

   D : Image.Metadata.Embedded.Data;
begin

   D := Image.Metadata.Embedded.Get (Command_Line.Argument (1));

   Text_IO.Put_Line ("Make          : " & To_String (D.Make));
   Text_IO.Put_Line ("Camera Model  : " & To_String (D.Camera_Model_Name));
   Text_IO.Put_Line ("Shutter Speed : " & To_String (D.Shutter_Speed_Value));
   Text_IO.Put_Line ("Aperture      : " & To_String (D.Aperture_Value));
   Text_IO.Put_Line ("Exposure Prog : " & To_String (D.Exposure_Program));
   Text_IO.Put_Line ("ISO           : " & To_String (D.ISO));
   Text_IO.Put_Line ("Create Date   : " & To_String (D.Create_Date));
   Text_IO.Put_Line ("Metering Mode : " & To_String (D.Metering_Mode));
   Text_IO.Put_Line ("Flash         : " & To_String (D.Flash));
   Text_IO.Put_Line ("Focal Length  : " & To_String (D.Focal_Length));
   Text_IO.Put_Line ("Exposure Mode : " & To_String (D.Exposure_Mode));
   Text_IO.Put_Line ("White Balance : " & To_String (D.White_Balance));
end DumpExif;
