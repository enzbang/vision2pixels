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

with Image.Metadata.Embedded;

package body Image_Tests.Embedded_Metadata is

   use Image;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;

   procedure Read (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Read metadata

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return String_Access is
      pragma Unreferenced (T);
   begin
      return new String'("Check image embedded metadata (exif/iptc)");
   end Name;

   ----------
   -- Read --
   ----------

   procedure Read (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Data : Metadata.Embedded.Data;
   begin
      Data := Metadata.Embedded.Get ("chat.jpg");

      Assert
        (Data.Make = "NIKON CORPORATION",
         "Wrong make : " & To_String (Data.Make));
      Assert
        (Data.Camera_Model_Name = "NIKON D200",
         "Wrong camera model : " & To_String (Data.Camera_Model_Name));
      Assert
        (Data.Shutter_Speed_Value = "1/100",
         "Wrong shutter speed : " & To_String (Data.Shutter_Speed_Value));
      Assert
        (Data.Aperture_Value = "5.6",
         "Wrong aperture : " & To_String (Data.Aperture_Value));
      Assert
        (Data.Exposure_Program = "Program AE",
         "Wrong exposure program : " & To_String (Data.Exposure_Program));
      Assert
        (Data.ISO = "800",
         "Wrong ISO : " & To_String (Data.ISO));
      Assert
        (Data.Create_Date = "2007:01:27 16:42:34",
         "Wrong create date : " & To_String (Data.Create_Date));
      Assert
        (Data.Metering_Mode = "Multi-segment",
         "Wrong metering mode : " & To_String (Data.Metering_Mode));
      Assert
        (Data.Flash = "No Flash",
         "Wrong flash : " & To_String (Data.Flash));
      Assert
        (Data.Focal_Length = "260.0mm",
         "Wrong focal length : " & To_String (Data.Focal_Length));
      Assert
        (Data.Exposure_Mode = "Auto",
         "Wrong exposure mode : " & To_String (Data.Exposure_Mode));
      Assert
        (Data.White_Balance = "Auto",
         "Wrong white balance : " & To_String (Data.White_Balance));
   end Read;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Read'Access, "Read metadata");
   end Register_Tests;

end Image_Tests.Embedded_Metadata;
