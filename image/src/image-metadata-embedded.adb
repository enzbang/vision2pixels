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
   pragma Warnings (Off);

   Exiftool   : aliased String := "exiftool";
   Cmd        : constant String := "cmd.exe";
   Cmd_Option : aliased String := "/c";
   Sh_Option  : aliased String := "sh";

   Make                : aliased String := "\nMake[^:]*: (.+)";
   Camera_Model_Name   : aliased String := "\nCamera Model Name[^:]*: (.+)";
   Shutter_Speed_Value : aliased String := "\nShutter Speed Value[^:]*: (.+)";
   Aperture_Value      : aliased String := "\nAperture Value[^:]*: (.+)";

   Regpat_Array        : constant Expect.Regexp_Array :=
                           (Make'Access,
                            Camera_Model_Name'Access,
                            Shutter_Speed_Value'Access,
                            Aperture_Value'Access);

   function "+"
     (Str : in String) return Unbounded_String renames To_Unbounded_String;

   ---------
   -- Get --
   ---------

   function Get (Filename : in String) return Data is
      Five_Secs : constant := 5_000;
      File      : aliased String := Filename;
      Pd        : Expect.Process_Descriptor;
      Matched   : Regpat.Match_Array (0 .. 3);
      Result    : Expect.Expect_Match;
      Metadata  : Data;
      use type Regpat.Match_Location;
   begin
      if OS.Is_Windows then
         Expect.Non_Blocking_Spawn
           (Pd, Cmd,
            (1 => Cmd_Option'Access,
             2 => Sh_Option'Access,
             3 => Exiftool'Access,
             4 => File'Unchecked_Access));
      else
         Expect.Non_Blocking_Spawn
           (Pd, Exiftool, (1 => File'Unchecked_Access));
      end if;

      Read_Metadata : loop
         begin
            Expect.Expect (Pd, Result, Regpat_Array, Matched, Five_Secs);
         exception
            when Expect.Process_Died =>
               exit Read_Metadata;
         end;

         case Result is
            when 1 =>
               Metadata.Make := +Expect.Expect_Out (Pd)
                 (Matched (1).First .. Matched (1).Last);

            when 2 =>
               Metadata.Camera_Model_Name := +Expect.Expect_Out (Pd)
                 (Matched (1).First .. Matched (1).Last);

            when 3 =>
               Metadata.Shutter_Speed_Value := +Expect.Expect_Out (Pd)
                 (Matched (1).First .. Matched (1).Last);

            when 4 =>
               Metadata.Aperture_Value := +Expect.Expect_Out (Pd)
                 (Matched (1).First .. Matched (1).Last);

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
