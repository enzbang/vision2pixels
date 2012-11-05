------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2012                          --
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

with Ada.Strings.Fixed;

with GNAT.Expect;
with GNAT.Regpat;
with GNAT.OS_Lib;

with Morzhol.Logs;
with Morzhol.OS;

package body Image.Metadata.Embedded is

   use Ada;
   use GNAT;

   use Morzhol.OS;

   Module       : constant Morzhol.Logs.Module_Name :=
                    "Image.Metadata.Embedded";

   Exiftool     : constant String := "exiftool";
   Exiftool_Opt : constant String := "-e";
   Cmd          : constant String := "cmd.exe";
   Cmd_Option   : constant String := "/c";
   Sh_Option    : constant String := "sh";

   Exiftool_Exe : constant OS_Lib.String_Access :=
                    OS_Lib.Locate_Exec_On_Path (Exiftool);
   Cmd_Exe      : constant OS_Lib.String_Access :=
                    OS_Lib.Locate_Exec_On_Path (Cmd);

   Suffix              : constant String := "  [^:]*: ([^\n\r]*)";

   Make                : aliased constant String :=
                           "\n(Make)" & Suffix;
   Camera_Model_Name   : aliased constant String :=
                           "\n(Camera Model Name)" & Suffix;
   Shutter_Speed_Value : aliased constant String :=
                           "\n(Shutter Speed Value|Exposure Time)" & Suffix;
   Aperture_Value      : aliased constant String :=
                           "\n(Aperture Value|F Number)" & Suffix;
   Exposure_Program    : aliased constant String :=
                           "\n(Exposure Program)" & Suffix;
   ISO                 : aliased constant String :=
                           "\n(ISO)" & Suffix;
   Create_Date         : aliased constant String :=
                           "\n(Create Date)" & Suffix;
   Metering_Mode       : aliased constant String :=
                           "\n(Metering Mode)" & Suffix;
   Flash               : aliased constant String :=
                           "\n(Flash)" & Suffix;
   Focal_Length        : aliased constant String :=
                           "\n(Focal Length)" & Suffix;
   Exposure_Mode       : aliased constant String :=
                           "\n(Exposure Mode)" & Suffix;
   White_Balance       : aliased constant String :=
                           "\n(White Balance)" & Suffix;

   --  Note that the pattern order in Regpat_Array is important as it must
   --  match the output of the exiftool.

   R_Make                : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Make);
   R_Camera_Model_Name   : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Camera_Model_Name);
   R_Shutter_Speed_Value : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Shutter_Speed_Value);
   R_Aperture_Value      : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Aperture_Value);
   R_Exposure_Program    : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Exposure_Program);
   R_ISO                 : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (ISO);
   R_Create_Date         : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Create_Date);
   R_Metering_Mode       : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Metering_Mode);
   R_Flash               : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Flash);
   R_Focal_Length        : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Focal_Length);
   R_Exposure_Mode       : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (Exposure_Mode);
   R_White_Balance       : constant Regpat.Pattern_Matcher :=
                             Regpat.Compile (White_Balance);
   ---------
   -- Get --
   ---------

   function Get (Filename : in String) return Data is

      function Run_Exiftool return String;
      --  Returns the output of exiftool

      procedure Check_Set
        (Output  : in     String;
         Result  :    out Unbounded_String;
         Pattern : in     Regpat.Pattern_Matcher);
      --  Check pattern in Output, set Result with corresponding value

      ---------------
      -- Check_Set --
      ---------------

      procedure Check_Set
        (Output  : in     String;
         Result  :    out Unbounded_String;
         Pattern : in     Regpat.Pattern_Matcher)
      is
         use type Regpat.Match_Location;
         Matches : Regpat.Match_Array (Regpat.Match_Count range 0 .. 3);
      begin
         Regpat.Match (Pattern, Output, Matches);

         if Matches (0) /= Regpat.No_Match then
            Result := To_Unbounded_String
              (Strings.Fixed.Trim
                 (Output (Matches (2).First .. Matches (2).Last),
                  Side => Strings.Both));
         end if;
      end Check_Set;

      ------------------
      -- Run_Exiftool --
      ------------------

      function Run_Exiftool return String is

         use type OS_Lib.String_Access;

         procedure Free_Args (Args : in out OS_Lib.Argument_List);

         ---------------
         -- Frea_Args --
         ---------------

         procedure Free_Args (Args : in out OS_Lib.Argument_List) is
         begin
            for K in Args'Range loop
               OS_Lib.Free (Args (K));
            end loop;
         end Free_Args;

         Status : aliased Integer;
         Res    : Unbounded_String;
         Args   : OS_Lib.Argument_List (1 .. 5);

      begin
         if Is_Windows and then Exiftool_Exe = null then
            --  Try with an Exiftool script
            Args := (1 => new String'(Cmd_Option),
                     2 => new String'(Sh_Option),
                     3 => new String'(Exiftool),
                     4 => new String'(Exiftool_Opt),
                     5 => new String'(Filename));

            OS_Lib.Normalize_Arguments (Args (1 .. 5));

            Res := To_Unbounded_String
              (Expect.Get_Command_Output
                 (Cmd_Exe.all, Args (1 .. 5),
                  Input  => "",
                  Status => Status'Access));

         elsif Exiftool_Exe = null then
            --  Exiftool not installed

            Morzhol.Logs.Write
              (Name    => Module,
               Kind    => Morzhol.Logs.Error,
               Content => "exiftool not installed");

         else
            Args (1 .. 2) := (1 => new String'(Exiftool_Opt),
                              2 => new String'(Filename));

            OS_Lib.Normalize_Arguments (Args (1 .. 2));

            Res := To_Unbounded_String
              (Expect.Get_Command_Output
                 (Exiftool_Exe.all, Args (1 .. 2),
                  Input  => "",
                  Status => Status'Access));
         end if;

         Free_Args (Args);
         return To_String (Res);
      exception
         when Expect.Invalid_Process =>
         Free_Args (Args);
         return "";
      end Run_Exiftool;

      Metadata : Data;

   begin
      Launch_External : declare
         Output : constant String := Run_Exiftool;
      begin
         if Output = "" then
            return No_Data;

         else
            Check_Set (Output, Metadata.Make, R_Make);
            Check_Set
              (Output, Metadata.Camera_Model_Name, R_Camera_Model_Name);
            Check_Set
              (Output, Metadata.Shutter_Speed_Value, R_Shutter_Speed_Value);
            Check_Set
              (Output, Metadata.Aperture_Value, R_Aperture_Value);
            Check_Set
              (Output, Metadata.Exposure_Program, R_Exposure_Program);
            Check_Set
              (Output, Metadata.ISO, R_ISO);
            Check_Set
              (Output, Metadata.Create_Date, R_Create_Date);
            Check_Set
              (Output, Metadata.Metering_Mode, R_Metering_Mode);
            Check_Set
              (Output, Metadata.Flash, R_Flash);
            Check_Set
              (Output, Metadata.Focal_Length, R_Focal_Length);
            Check_Set
              (Output, Metadata.Exposure_Mode, R_Exposure_Mode);
            Check_Set
              (Output, Metadata.White_Balance, R_White_Balance);
         end if;
      end Launch_External;

      return Metadata;
   end Get;

end Image.Metadata.Embedded;
