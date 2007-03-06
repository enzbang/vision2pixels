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

--  Reading the embedded image metadata is done using an external tool. This
--  implementation is based on ExifTools.

with GNAT.Expect;
with GNAT.Regpat;

with OS;

package body Image.Metadata.Embedded is

   use Ada;
   use GNAT;

   Exiftool     : aliased String := "exiftool";
   Exiftool_Opt : aliased String := "-e";
   Cmd          : constant String := "cmd.exe";
   Cmd_Option   : aliased String := "/c";
   Sh_Option    : aliased String := "sh";

   Suffix              : constant String := "  [^:]*: (.+)";

   Make                : aliased String := "\nMake" & Suffix;
   Camera_Model_Name   : aliased String := "\nCamera Model Name" & Suffix;
   Shutter_Speed_Value : aliased String := "\nShutter Speed Value" & Suffix;
   Aperture_Value      : aliased String := "\nAperture Value" & Suffix;
   Exposure_Program    : aliased String := "\nExposure Program" & Suffix;
   ISO                 : aliased String := "\nISO" & Suffix;
   Create_Date         : aliased String := "\nCreate Date" & Suffix;
   Metering_Mode       : aliased String := "\nMetering Mode" & Suffix;
   Flash               : aliased String := "\nFlash" & Suffix;
   Focal_Length        : aliased String := "\nFocal Length " & Suffix;
   Exposure_Mode       : aliased String := "\nExposure Mode" & Suffix;
   White_Balance       : aliased String := "\nWhite Balance" & Suffix;

   --  Note that the pattern order in Regpat_Array is important as it must
   --  match the output of the exiftool.

   Regpat_Array        : constant Expect.Regexp_Array :=
                           (Make'Access,
                            Camera_Model_Name'Access,
                            Shutter_Speed_Value'Access,
                            Aperture_Value'Access,
                            Exposure_Program'Access,
                            ISO'Access,
                            Create_Date'Access,
                            Metering_Mode'Access,
                            Flash'Access,
                            Focal_Length'Access,
                            Exposure_Mode'Access,
                            White_Balance'Access);

   ---------
   -- Get --
   ---------

   function Get (Filename : in String) return Data is

      function First return Unbounded_String;
      --  Returns first matching string

      Five_Secs : constant := 5_000;
      File      : aliased String := Filename;
      Pd        : Expect.Process_Descriptor;
      Matched   : Regpat.Match_Array (0 .. 3);
      Result    : Expect.Expect_Match;
      Metadata  : Data;

      -----------
      -- First --
      -----------

      function First return Unbounded_String is
      begin
         return To_Unbounded_String
           (Expect.Expect_Out (Pd) (Matched (1).First .. Matched (1).Last));
      end First;

   begin
      begin
         if OS.Is_Windows then
            Expect.Non_Blocking_Spawn
              (Pd, Cmd,
               (1 => Cmd_Option'Access,
                2 => Sh_Option'Access,
                3 => Exiftool'Access,
                4 => Exiftool_Opt'Access,
                5 => File'Unchecked_Access));
         else
            Expect.Non_Blocking_Spawn
              (Pd, Exiftool,
               (1 => Exiftool_Opt'Access, 2 => File'Unchecked_Access));
         end if;
      exception
         when Expect.Invalid_Process =>
            --  Exiftool not installed, ignore
            return Metadata;
      end;

      Read_Metadata : loop
         begin
            Expect.Expect (Pd, Result, Regpat_Array, Matched, Five_Secs);
         exception
            when Expect.Process_Died =>
               exit Read_Metadata;
         end;

         case Result is
            when  1 => Metadata.Make := First;
            when  2 => Metadata.Camera_Model_Name := First;
            when  3 => Metadata.Shutter_Speed_Value := First;
            when  4 => Metadata.Aperture_Value := First;
            when  5 => Metadata.Exposure_Program := First;
            when  6 => Metadata.ISO := First;
            when  7 => Metadata.Create_Date := First;
            when  8 => Metadata.Metering_Mode := First;
            when  9 => Metadata.Flash := First;
            when 10 => Metadata.Focal_Length := First;
            when 11 => Metadata.Exposure_Mode := First;
            when 12 => Metadata.White_Balance := First;

            when Expect.Expect_Timeout =>
               exit Read_Metadata;

            when others =>
               null;
         end case;
      end loop Read_Metadata;

      Expect.Close (Pd);

      return Metadata;
   end Get;

end Image.Metadata.Embedded;
