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

with Image.Metadata.Geographic;

package body Image_Tests.Metadata is

   use Image.Metadata.Geographic;
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

      Pos_Lat  : Latitude;
      Pos_Long : Longitude;

      Get_Lat  : Geo_Coordinate;
      Get_Long : Geo_Coordinate;
   begin
      --  48.864263, 2.397927

      Pos_Lat.Format (Geo_Lat);
      Pos_Long.Format (Geo_Long);

      Assert (Pos_Lat = Latitude'(48, 51, 51, N),
              "Error in latitude format for (48, 51, 51, N)");

      Get_Lat := To_Geo_Coordinate (Pos_Lat);

      Assert (Get_Lat = Geo_Lat,
              "To_Geo_Coordinate error for 48° 51' 51'' N");

      Assert (Pos_Long = Longitude'(2, 23, 53, E),
              "Error in longitude format (2, 23, 53, E)");

      Get_Long := To_Geo_Coordinate (Pos_Long);

      Assert (Get_Long = Get_Lat,
              "To_Geo_Coordinate error for 2° 23' 53'' E");

      Assert (Image.Metadata.Geographic.Image (Pos_Lat) = "N 48° 51' 51",
              "Error with " & Image.Metadata.Geographic.Image (Pos_Lat));

      Assert (Image.Metadata.Geographic.Image (Pos_Long) = "E 2° 23' 53",
              "Error with " & Image.Metadata.Geographic.Image (Pos_Long));

      --  -87.728055, -40.4461110

      Geo_Long  := -87.728055;
      Geo_Lat   := -40.4461110;

      Pos_Lat.Format (Geo_Lat);
      Pos_Long.Format (Geo_Long);

      Assert (Pos_Lat = Latitude'(40, 26, 46, S),
              "Error in latitude format for (40, 26, 46, S)");

      Assert (Pos_Long = Longitude'(87, 43, 41, W),
              "Error in longitude format for (87, 43, 41, W)");

      Get_Lat  := To_Geo_Coordinate (Pos_Lat);
      Get_Long := To_Geo_Coordinate (Pos_Long);

      Assert (Get_Lat = Geo_Lat,
              "To_Geo_Coordinate error for 40° 26' 46'' S");

      Assert (Get_Long = Geo_Long,
              "To_Geo_Coordinate error for 87° 43' 41'' W");

      Assert (Image.Metadata.Geographic.Image (Pos_Lat) = "S 40° 26' 46",
              "Error with " & Image.Metadata.Geographic.Image (Pos_Lat));

      Assert (Image.Metadata.Geographic.Image (Pos_Long) = "W 87° 43' 41",
              "Error with " & Image.Metadata.Geographic.Image (Pos_Long));
   end Geo_Format;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return New_String ("Check image metadata functions");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Geo_Format'Access, "Geo format");
   end Register_Tests;

end Image_Tests.Metadata;
