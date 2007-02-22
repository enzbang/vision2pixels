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

package Image.Metadata.Geographic is


   type Degree is range -180 .. 180;
   type Minute is mod 60;
   type Second is mod 60;
   type Geo_Coordinate is delta 10.0 ** (-6) range -180.0 .. 180.0;

   type Latitude_Direction is (N, S);
   type Longitude_Direction is (E, W);

   type Geo_Position is tagged record
      C_Degree : Degree;
      C_Minute : Minute;
      C_Second : Second;
   end record;
   --  Human readable geoposition

   function "=" (Left, Right : in Geo_Coordinate) return Boolean;

   procedure Format
     (Position   : in out Geo_Position'Class;
      Coordinate : in     Geo_Coordinate);
   --  Format a geo coordinate into human readable Geo_Position

   function To_Geo_Coordinate
     (Position : in Geo_Position'Class) return Geo_Coordinate;
   --  Convert a Geo_Position to Geo_Coordinate

   procedure Format_Direction
     (Position   : in out Geo_Position;
      Coordinate : in     Geo_Coordinate) is null;
   --  Adds direction to Geo_Position

   procedure Set_Sign
     (Coordinate : in out Geo_Coordinate; Position : in Geo_Position) is null;
   --  Set Geo_Coordinate sign according to Position direction

   function Image (Position : in Geo_Position) return String;
   --  Returns the image of a geo_position

   --  Latitude

   type Latitude is new Geo_Position with record
      C_Direction : Latitude_Direction;
   end record;

   overriding procedure Format_Direction
     (Position   : in out Latitude;
      Coordinate : in     Geo_Coordinate);

   function Image (Position : in Latitude) return String;
   --  Returns latitude image

   overriding procedure Set_Sign
     (Coordinate : in out Geo_Coordinate; Position : in Latitude);

   --  Longitude

   type Longitude is new Geo_Position with record
      C_Direction : Longitude_Direction;
   end record;

   overriding procedure Format_Direction
     (Position   : in out Longitude;
      Coordinate : in     Geo_Coordinate);

   function Image (Position : in Longitude) return String;
   --  Returns Longitude image

   overriding procedure Set_Sign
     (Coordinate : in out Geo_Coordinate; Position : in Longitude);

end Image.Metadata.Geographic;
