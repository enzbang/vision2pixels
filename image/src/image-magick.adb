------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
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

with G2F.Image_Resize;

package body Image.Magick is

   use G2F.Image_Resize;
   use G2F.IO;

   ---------------
   -- Thumbnail --
   ---------------

   function Thumbnail
     (Img  : in G2F.Image_Ptr;
      Size : in G2F.IO.Image_Size) return G2F.Image_Ptr
   is
      Original_Size : constant Image_Size := Get_Image_Size (Img);
      X_Length      : Image_Size_T;
      Y_Length      : Image_Size_T;
      Thumb         : G2F.Image_Ptr;
   begin
      if Original_Size.X / Size.X > Original_Size.Y / Size.Y then
         X_Length := Size.X;
         Y_Length := Original_Size.Y * X_Length / Original_Size.X;
      else
         Y_Length := Size.Y;
         X_Length := Original_Size.X * Y_Length / Original_Size.Y;
      end if;

      Thumb := G2F.Image_Resize.Thumbnail_Image (Img, (X_Length, Y_Length));

      return Thumb;
   end Thumbnail;

end Image.Magick;
