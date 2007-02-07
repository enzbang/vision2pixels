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

package Image.Metadata is

   type Degree is range -180 .. 180;
   type Minute is mod 60;
   --   type Second is delta 10.0 ** (-2) range 0.00 .. 59.99;
   type Second is mod 60;
   type Geo_Coordinate is delta 10.0 ** (-6) range -180.0 .. 180.0;

   type Latitude_Direction is (N, S);
   type Longitude_Direction is (E, W);

   type Geo_Position is tagged record
      C_Degree  : Degree;
      C_Minute  : Minute;
      C_Second  : Second;
   end record;
   --  Human readable geoposition

   function "=" (I, J : in Geo_Coordinate) return Boolean;

   procedure Format
     (C         : in Geo_Coordinate;
      Formatted : in out Geo_Position'class);
   --  Format a geo coordinate into human readable Geo_Position

   function To_Geo_Coordinate
     (Formatted : in Geo_Position'Class)
      return Geo_Coordinate;
   --  Convert a Geo_Position to Geo_Coordinate


   procedure Format_Direction
     (C         : in Geo_Coordinate;
      Formatted : in out Geo_Position) is null;
   --  Adds direction to Geo_Position

   procedure Set_Sign (C : in out Geo_Coordinate; G : in Geo_Position) is null;
   --  Set Geo_Coordinate sign according to G direction

   --  Latitude

   type Latitude is new Geo_Position with record
      C_Direction : Latitude_Direction;
   end record;

   overriding
   procedure Format_Direction
     (C         : in Geo_Coordinate;
      Formatted : in out Latitude);

   overriding
   procedure Set_Sign (C : in out Geo_Coordinate; L : in Latitude);

   --  Longitude

   type Longitude is new Geo_Position with record
      C_Direction : Longitude_Direction;
   end record;

   overriding
   procedure Format_Direction
     (C         : in Geo_Coordinate;
      Formatted : in out Longitude);

   overriding
   procedure Set_Sign (C : in out Geo_Coordinate; L : in Longitude);

end Image.Metadata;
