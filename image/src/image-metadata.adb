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

package body Image.Metadata is

   ---------
   -- "=" --
   ---------

   function "=" (I, J : in Geo_Coordinate) return Boolean is
   begin
      if I - J < 0.0001 then
         return True;
      else
         return False;
      end if;
   end "=";

   ------------
   -- Format --
   ------------

   procedure Format
     (C         : in     Geo_Coordinate;
      Formatted : in out Geo_Position'Class)
   is
      Diff : Geo_Coordinate;
   begin
      Format_Direction (C, Formatted);

      --  The sign information is in Formatted.C_Direction
      --  Then work only with positive coordinate

      if C < 0.0 then
         Diff := -C;
      else
         Diff := C;
      end if;

      Formatted.C_Degree := Degree (Float'Truncation (Float (Diff)));

      Diff := Diff - Geo_Coordinate (Formatted.C_Degree);

      Formatted.C_Minute := Minute (Float'Truncation (Float (Diff * 60)));
      Diff := Diff - Geo_Coordinate (Formatted.C_Minute) / 60.0;

      Formatted.C_Second := Second (Diff * 3600);
   end Format;

   ----------------------
   -- Format_Direction --
   ----------------------

   overriding procedure Format_Direction
     (C         : in     Geo_Coordinate;
      Formatted : in out Latitude) is
   begin
      if C > Geo_Coordinate (0) then
         Formatted.C_Direction := N;
      else
         Formatted.C_Direction := S;
      end if;
   end Format_Direction;


   overriding procedure Format_Direction
     (C         : in     Geo_Coordinate;
      Formatted : in out Longitude) is
   begin
      if C > Geo_Coordinate (0) then
         Formatted.C_Direction := E;
      else
         Formatted.C_Direction := W;
      end if;
   end Format_Direction;

   --------------
   -- Set_Sign --
   --------------

   overriding procedure Set_Sign
     (C : in out Geo_Coordinate; L : in Latitude) is
   begin
      if L.C_Direction = S then
         C := -C;
      end if;
   end Set_Sign;

   overriding procedure Set_Sign
     (C : in out Geo_Coordinate; L : in Longitude) is
   begin
      if L.C_Direction = W then
         C := -C;
      end if;
   end Set_Sign;

   -----------------------
   -- To_Geo_Coordinate --
   -----------------------

   function To_Geo_Coordinate
     (Formatted : in Geo_Position'Class) return Geo_Coordinate
   is
      Result : Geo_Coordinate;
   begin
      Result := Geo_Coordinate (Float (Formatted.C_Second) / 3600.0);
      Result := Result + Geo_Coordinate (Float (Formatted.C_Minute) / 60.0);
      Result := Result + Geo_Coordinate (Formatted.C_Degree);

      Set_Sign (Result, Formatted);
      return Result;
   end To_Geo_Coordinate;

end Image.Metadata;
