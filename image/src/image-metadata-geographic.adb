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

package body Image.Metadata.Geographic is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Geo_Coordinate) return Boolean is
   begin
      if Left - Right < 0.0001 then
         return True;
      else
         return False;
      end if;
   end "=";

   ------------
   -- Format --
   ------------

   procedure Format
     (Position   : in out Geo_Position'Class;
      Coordinate : in     Geo_Coordinate)
   is
      Diff : Geo_Coordinate;
   begin
      Position.Format_Direction (Coordinate);

      --  The sign information is in Formatted.C_Direction
      --  Then work only with positive coordinate

      if Coordinate < 0.0 then
         Diff := -Coordinate;
      else
         Diff := Coordinate;
      end if;

      Position.C_Degree := Degree (Float'Truncation (Float (Diff)));

      Diff := Diff - Geo_Coordinate (Position.C_Degree);

      Position.C_Minute := Minute (Float'Truncation (Float (Diff * 60)));
      Diff := Diff - Geo_Coordinate (Position.C_Minute) / 60.0;

      Position.C_Second := Second (Diff * 3600);
   end Format;

   ----------------------
   -- Format_Direction --
   ----------------------

   overriding procedure Format_Direction
     (Position   : in out Latitude;
      Coordinate : in     Geo_Coordinate) is
   begin
      if Coordinate > 0.0 then
         Position.C_Direction := N;
      else
         Position.C_Direction := S;
      end if;
   end Format_Direction;


   overriding procedure Format_Direction
     (Position   : in out Longitude;
      Coordinate : in     Geo_Coordinate) is
   begin
      if Coordinate > 0.0 then
         Position.C_Direction := E;
      else
         Position.C_Direction := W;
      end if;
   end Format_Direction;

   -----------
   -- Image --
   -----------

   function Image (Position : in Geo_Position) return String is
   begin
      return Degree'Image (Position.C_Degree) & "°"
        & Minute'Image (Position.C_Minute) & "'"
        & Second'Image (Position.C_Second);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Position : in Latitude) return String is
   begin
      return Latitude_Direction'Image (Position.C_Direction)
        & Image (Geo_Position (Position));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Position : in Longitude) return String is
   begin
      return Longitude_Direction'Image (Position.C_Direction)
        & Image (Geo_Position (Position));
   end Image;

   --------------
   -- Set_Sign --
   --------------

   overriding procedure Set_Sign
     (Coordinate : in out Geo_Coordinate; Position : in Latitude) is
   begin
      if Position.C_Direction = S then
         Coordinate := -Coordinate;
      end if;
   end Set_Sign;

   overriding procedure Set_Sign
     (Coordinate : in out Geo_Coordinate; Position : in Longitude) is
   begin
      if Position.C_Direction = W then
         Coordinate := -Coordinate;
      end if;
   end Set_Sign;

   -----------------------
   -- To_Geo_Coordinate --
   -----------------------

   function To_Geo_Coordinate
     (Position : in Geo_Position'Class) return Geo_Coordinate
   is
      Result : Geo_Coordinate;
   begin
      Result := Geo_Coordinate (Float (Position.C_Second) / 3600.0);
      Result := Result + Geo_Coordinate (Float (Position.C_Minute) / 60.0);
      Result := Result + Geo_Coordinate (Position.C_Degree);

      Set_Sign (Result, Position);
      return Result;
   end To_Geo_Coordinate;

end Image.Metadata.Geographic;
