------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2008                          --
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
with Ada.Directories;

with MagickWand;

package Image.Data is

   use Ada;
   use Ada.Strings.Unbounded;

   Image_Error : exception;
   --  Raised for all errors reported by Image.Data

   type Image_Data is tagged private;

   type Image_Dimension is record
      Width         : MagickWand.Size;
      Height        : MagickWand.Size;
      Medium_Width  : MagickWand.Size;
      Medium_Height : MagickWand.Size;
      Thumb_Width   : MagickWand.Size;
      Thumb_Height  : MagickWand.Size;
      Size          : Directories.File_Size;
   end record;

   Null_Dimension : constant Image_Dimension;

   type Image_Init_Status is
     (Exceed_Max_Image_Dimension, Exceed_Max_Size, Image_Created);

   procedure Init
     (Img      : in out Image_Data;
      Root_Dir : in     String;
      Filename : in     String);
   --  Set image filename, read image info and create thumbnail
   --  Generate image and thumb filename under Root_Dir.

   procedure Store_Avatar
     (Img      : in out Image_Data;
      Root_Dir : in     String;
      Filename : in     String);
   --  Store a new avatar

   function Filename (Img : in Image_Data) return String;
   --  Returns image filename

   function Dimension (Img : in Image_Data) return Image_Dimension;
   --  Returns image max dimension

   function Init_Status (Img : in Image_Data) return Image_Init_Status;
   --  Returns image init_status

   function Default_Max_Dimension return Image_Dimension;
   --  Returns default maximum dimension

private

   Null_Dimension : constant Image_Dimension :=
                      Image_Dimension'(Width => 0, Height => 0,
                                       Medium_Width => 0, Medium_Height => 0,
                                       Thumb_Width => 0, Thumb_Height => 0,
                                       Size => 0);

   type Image_Data is tagged record
      Image       : MagickWand.Object;
      Category    : Unbounded_String;
      Dimension   : Image_Dimension;
      Init_Status : Image_Init_Status;
   end record;

end Image.Data;
