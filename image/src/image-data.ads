------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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
with Ada.Strings.Unbounded;

with G2F.IO;

package Image.Data is

   use Ada;
   use Ada.Strings.Unbounded;

   Image_Error : exception;
   --  Raised for all errors reported by Image.Data

   type Image_Data is tagged private;

   type Image_Init_Status is
     (Exceed_Max_Image_Dimension,
      Exceed_Max_Size,
      Image_Created);

   Thumbnail_Size : constant G2F.IO.Image_Size := (150, 150);

   procedure Init
      (Img      : in out Image_Data;
       Filename : in     String;
       Category : in     String);
   --  Set image filename, read image info and create thumbnail

   function Filename (Img : in Image_Data) return String;
   --  Returns image filename

   function Width (Img : in Image_Data) return Integer;
   --  Returns image width

   function Height (Img : in Image_Data) return Integer;
   --  Returns image height

   function Size (Img : in Image_Data) return Integer;
   --  Returns image size

   function Init_Status (Img : in Image_Data) return Image_Init_Status;

private

   type Image_Data is new Finalization.Controlled with record
      Info_Ptr    : G2F.Image_Info_Ptr;
      Image_Ptr   : G2F.Image_Ptr;
      Category    : Unbounded_String;
      Width       : Integer;
      Height      : Integer;
      Size        : Integer;
      Init_Status : Image_Init_Status;
   end record;

   overriding procedure Initialize (Img : in out Image_Data);
   --  Initialize Image_Ptr and Image_Info_Ptr structures

   overriding procedure Finalize (Img : in out Image_Data);
   --  Destroys Image

end Image.Data;
