------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2009                          --
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

package body Image.Magick is

   use type MagickWand.Size;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Resized_Image : in out MagickWand.Object;
      Img           : in     MagickWand.Object;
      Size          : in     MagickWand.Image_Size)
   is
      Original_Size : constant MagickWand.Image_Size :=
                        (Width  => Img.Get_Width,
                         Height => Img.Get_Height);
      X_Length      : MagickWand.Size;
      Y_Length      : MagickWand.Size;

   begin
      Resized_Image.Clone (From => Img);

      --  Resize only if bigger than expected

      if Original_Size.Width < Size.Width
        and then Original_Size.Height < Size.Height
      then
         X_Length := Original_Size.Width;
         Y_Length := Original_Size.Height;

      else
         if Original_Size.Width / Size.Width
           > Original_Size.Height / Size.Height
         then
            X_Length := Size.Width;
            Y_Length := Original_Size.Height * X_Length / Original_Size.Width;
         else
            Y_Length := Size.Height;
            X_Length := Original_Size.Width * Y_Length / Original_Size.Height;
         end if;
      end if;

      Resized_Image.Resize
        (Width  => X_Length,
         Height => Y_Length,
         Filter => MagickWand.LanczosFilter,
         Blur   => 1.0);
   end Resize;

   ---------------
   -- Thumbnail --
   ---------------

   procedure Thumbnail
     (Thumb : in out MagickWand.Object;
      Img   : in     MagickWand.Object;
      Size  : in     MagickWand.Image_Size)
   is
      Original_Size : constant MagickWand.Image_Size :=
                        (Width  => Img.Get_Width,
                         Height => Img.Get_Height);
      X_Length      : MagickWand.Size;
      Y_Length      : MagickWand.Size;
   begin
      Thumb.Clone (From => Img);

      if Original_Size.Width / Size.Width
        > Original_Size.Height / Size.Height
      then
         X_Length := Size.Height;
         Y_Length := Original_Size.Height * X_Length / Original_Size.Width;
      else
         Y_Length := Size.Height;
         X_Length := Original_Size.Width * Y_Length / Original_Size.Height;
      end if;

      Thumb.Resize
        (Width  => X_Length,
         Height => Y_Length,
         Filter => MagickWand.LanczosFilter,
         Blur   => 1.0);
   end Thumbnail;

begin
   MagickWand.Genesis;
end Image.Magick;
