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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with G2F.Image_Resize;
with Ada.Text_IO;

package body Image.Magick is

   use G2F.Image_Resize;
   use G2F.IO;
   use Ada.Text_IO;

   function Thumbnail
     (Img   : G2F.Image_Ptr;
      Size  : G2F.IO.Image_Size)
      return G2F.Image_Ptr
   is
      Original_Size : constant Image_Size := Get_Image_Size (Img);
      X_Length      : Image_Size_T;
      Y_Length      : Image_Size_T;
      Thumb : G2F.Image_Ptr;
   begin
      if Original_Size.X / Size.X > Original_Size.Y / Size.Y then
         X_Length := Size.X;
         Y_Length := Original_Size.Y * Size.X / Original_Size.X;
      else
         X_Length := Original_Size.X * Y_Length / Original_Size.Y;
         Y_Length := Size.Y;
      end if;

      Put_Line ("OX" & Image_Size_T'Image (Original_Size.X));
      Put_Line ("X" & Image_Size_T'Image (X_Length));
      Put_Line ("Y" & Image_Size_T'Image (Y_Length));


      Thumb := G2F.Image_Resize.Resize_Image
        (Img, (X_Length, Y_Length), G2F.Image_Resize.Mitchell, 0.0);
      return Thumb;
   end Thumbnail;

end Image.Magick;
