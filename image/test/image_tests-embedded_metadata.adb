------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with Ada.Strings.Unbounded;

with AUnit.Assertions;

with Image.Metadata.Embedded;

package body Image_Tests.Embedded_Metadata is

   use Ada.Strings.Unbounded;
   use Image;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;

   procedure Read (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Read metadata

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format ("Check image embedded metadata (exif/iptc)");
   end Name;

   ----------
   -- Read --
   ----------

   procedure Read (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Check_Expected
        (Photo : in String;
         Make, Model, Shutter_Speed, Aperture, Exposure_Program,
         ISO, Create_Date, Metering_Mode,
         Flash, Focal, Exposure_Mode, White_Balance : in String);

      --------------------
      -- Check_Expected --
      --------------------

      procedure Check_Expected
        (Photo : in String;
         Make, Model, Shutter_Speed, Aperture, Exposure_Program,
         ISO, Create_Date, Metering_Mode,
         Flash, Focal, Exposure_Mode, White_Balance : in String)
      is
         Data : Metadata.Embedded.Data;
      begin
         Data := Metadata.Embedded.Get (Photo);

         Assert
           (Data.Make = Make,
            Photo & " - Wrong make : " & To_String (Data.Make));
         Assert
           (Data.Camera_Model_Name = Model,
            Photo & " - Wrong camera model : "
            & To_String (Data.Camera_Model_Name));
         Assert
           (Data.Shutter_Speed_Value = Shutter_Speed,
            Photo & " - Wrong shutter speed : "
            & To_String (Data.Shutter_Speed_Value));
         Assert
           (Data.Aperture_Value = Aperture,
            Photo & " - Wrong aperture : " & To_String (Data.Aperture_Value));
         Assert
           (Data.Exposure_Program = Exposure_Program,
            Photo & " - Wrong exposure program : "
            & To_String (Data.Exposure_Program));
         Assert
           (Data.ISO = ISO,
            Photo & " - Wrong ISO : " & To_String (Data.ISO));
         Assert
           (Data.Create_Date = Create_Date,
            Photo & " - Wrong create date : " & To_String (Data.Create_Date));
         Assert
           (Data.Metering_Mode = Metering_Mode,
            Photo & " - Wrong metering mode : "
            & To_String (Data.Metering_Mode));
         Assert
           (Data.Flash = Flash,
            Photo & " - Wrong flash : " & To_String (Data.Flash));
         Assert
           (Data.Focal_Length = Focal,
            Photo & " - Wrong focal length : "
            & To_String (Data.Focal_Length));
         Assert
           (Data.Exposure_Mode = Exposure_Mode,
            Photo & " - Wrong exposure mode : "
            & To_String (Data.Exposure_Mode));
         Assert
           (Data.White_Balance = White_Balance,
            Photo & " - Wrong white balance : "
            & To_String (Data.White_Balance));
      end Check_Expected;

   begin
      Check_Expected
        (Photo            => "chat.jpg",
         Make             => "NIKON CORPORATION",
         Model            => "NIKON D200",
         Shutter_Speed    => "1/100",
         Aperture         => "5.6",
         Exposure_Program => "Program AE",
         ISO              => "800",
         Create_Date      => "2007:01:27 16:42:34",
         Metering_Mode    => "Multi-segment",
         Flash            => "No Flash",
         Focal            => "260.0 mm",
         Exposure_Mode    => "Auto",
         White_Balance    => "Auto");

      --  pbexif.jpg

      Check_Expected
        (Photo            => "pbsexif.jpg",
         Make             => "NIKON CORPORATION",
         Model            => "NIKON D80",
         Shutter_Speed    => "30",
         Aperture         => "16.0",
         Exposure_Program => "Aperture-priority AE",
         ISO              => "100",
         Create_Date      => "2008:08:21 20:33:42",
         Metering_Mode    => "Multi-segment",
         Flash            => "0",
         Focal            => "18.0 mm",
         Exposure_Mode    => "Auto",
         White_Balance    => "Auto");
   end Read;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Read'Access, "Read metadata");
   end Register_Tests;

end Image_Tests.Embedded_Metadata;
