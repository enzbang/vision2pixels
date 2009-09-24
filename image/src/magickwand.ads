------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with Ada.Finalization;

private with System;
with Interfaces.C;

package MagickWand is

   pragma Linker_Options ("-lMagickCore");
   pragma Linker_Options ("-LMagickWand");

   MagickError : exception;

   type Object is tagged private;

   procedure Clone (New_Object : in out Object; From : in Object);
   --  Clone an object

   procedure Genesis;
   pragma Import (C, Genesis, "MagickWandGenesis");
   --  wand/MagickWand.h:189:3

   procedure Terminus;
   pragma Import (C, Terminus, "MagickWandTerminus");
   --  wand/MagickWand.h:190:3

   type Size is new Long_Integer;

   type Image_Size is record
      Width  : Size;
      Height : Size;
   end record;

   function Get_Filename (O : Object) return String;
   function Get_Width (O : Object) return Size;
   function Get_Height (O : Object) return Size;

   procedure Set_Compression_Quality (O : Object; Value : Float := 100.0);
   procedure Set_Filename (O : Object; Filename : String);

   procedure Write (O : Object; Filename : String);
   procedure Read (O : in out Object; Filename : String);

   subtype FilterTypes is Interfaces.C.unsigned;
   UndefinedFilter : constant FilterTypes := 0;
   PointFilter     : constant FilterTypes := 1;
   BoxFilter       : constant FilterTypes := 2;
   TriangleFilter  : constant FilterTypes := 3;
   HermiteFilter   : constant FilterTypes := 4;
   HanningFilter   : constant FilterTypes := 5;
   HammingFilter   : constant FilterTypes := 6;
   BlackmanFilter  : constant FilterTypes := 7;
   GaussianFilter  : constant FilterTypes := 8;
   QuadraticFilter : constant FilterTypes := 9;
   CubicFilter     : constant FilterTypes := 10;
   CatromFilter    : constant FilterTypes := 11;
   MitchellFilter  : constant FilterTypes := 12;
   LanczosFilter   : constant FilterTypes := 13;
   BesselFilter    : constant FilterTypes := 14;
   SincFilter      : constant FilterTypes := 15;
   KaiserFilter    : constant FilterTypes := 16;
   WelshFilter     : constant FilterTypes := 17;
   ParzenFilter    : constant FilterTypes := 18;
   LagrangeFilter  : constant FilterTypes := 19;
   BohmanFilter    : constant FilterTypes := 20;
   BartlettFilter  : constant FilterTypes := 21;
   SentinelFilter  : constant FilterTypes := 22;
   --  magick/resample.h:52:3

   procedure Resize
     (O      : in Object;
      Width  : in Size;
      Height : in Size;
      Filter : in FilterTypes;
      Blur   : in Long_Float := 1.0);

private

   type Object is new Ada.Finalization.Controlled with record
      Wand : System.Address := System.Null_Address;
   end record;

   overriding procedure Finalize (O : in out Object);

   subtype MagickBooleanType is Interfaces.C.unsigned;
   MagickFalse : constant MagickBooleanType := 0;
   MagickTrue  : constant MagickBooleanType := 1;
   --  magick/magick-type.h:175:3

   MaxTextExtent : constant := 4096;
   --  wand/MagickWand.h:132

end MagickWand;
