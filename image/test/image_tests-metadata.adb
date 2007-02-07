------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                          Copyright (C) 2007                              --
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

with AUnit.Test_Cases.Registration;
with AUnit.Assertions;

with Image.Metadata;

package body Image_Tests.Metadata is

   use Ada;
   use Image.Metadata;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;

   procedure Geo_Format (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check geo format function

   ----------------
   -- Geo_Format --
   ----------------

   procedure Geo_Format (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Geo_Lat   : Geo_Coordinate := 48.864263;
      Geo_Long  : Geo_Coordinate := 2.397927;

      Formatted_Latitude  : Latitude
        := (Degree (0), Minute (0), Second (0), S);
      Formatted_Longitude : Longitude
        := (Degree (0), Minute (0), Second (0), W);

      Get_Lat  : Geo_Coordinate;
      Get_Long : Geo_Coordinate;
   begin

      --  48.864263, 2.397927

      Format (Geo_Lat, Formatted_Latitude);
      Format (Geo_Long, Formatted_Longitude);

      Assert (Formatted_Latitude = (48, 51, 51, N),
              "Error in latitude format for (48, 51, 51, N)");

      Get_Lat := To_Geo_Coordinate (Formatted_Latitude);

      Assert (Get_Lat = Geo_Lat,
              "To_Geo_Coordinate error for 48° 51' 51'' N");

      Assert (Formatted_Longitude = (2, 23, 53, E),
              "Error in longitude format (2, 23, 53, E)");

      Get_Long := To_Geo_Coordinate (Formatted_Longitude);

      Assert (Get_Long = Get_Lat,
              "To_Geo_Coordinate error for 2° 23' 52'' E");

      --  -87.728055, -40.4461110

      Geo_Long  := -87.728055;
      Geo_Lat   := -40.4461110;

      Format (Geo_Lat, Formatted_Latitude);
      Format (Geo_Long, Formatted_Longitude);

      Assert (Formatted_Latitude = (40, 26, 46, S),
              "Error in latitude format for (40, 26, 46, S)");

      Assert (Formatted_Longitude = (87, 43, 41, W),
              "Error in longitude format for (87, 43, 41, W)");

      Get_Lat  := To_Geo_Coordinate (Formatted_Latitude);
      Get_Long := To_Geo_Coordinate (Formatted_Longitude);

      Assert (Get_Lat = Geo_Lat,
              "To_Geo_Coordinate error for 40° 26' 46'' N");

      Assert (Get_Long = Geo_Long,
              "To_Geo_Coordinate error for 87° 43' 41'' W");

   end Geo_Format;


   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return String_Access is
   pragma Unreferenced (T);
   begin
      return new String'("Check image metadata functions");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Geo_Format'Access, "Geo format");
   end Register_Tests;

end Image_Tests.Metadata;
