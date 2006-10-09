----------------------------------------------------------
----------------------
--                              G2f_Io                                      --
--                                                                          --
--                         Copyright (C) 2004                               --
--                            Ali Bendriss                                  --
--                                                                          --
--  Author: Ali Bendriss                                                    --
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
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with G2f.Io;

package G2f.Image_Resize is

   Resize_Error : exception;

   type T_Blur is new Float;
   for T_Blur'Size use 64;

   type Resize_Filter is (
      Point,
      Box,
      Triangle,
      Hermite,
      Hanning,
      Blackman,
      Gaussian,
      Quandratic,
      Cubic,
      Catrom,
      Mitchell,
      Lanczos,
      Bessel,
      Sinc);

   for Resize_Filter use
     (Bessel     => 14,
      Blackman   => 7,
      Box        => 2,
      Catrom     => 11,
      Cubic      => 10,
      Gaussian   => 8,
      Hanning    => 5,
      Hermite    => 4,
      Lanczos    => 13,
      Mitchell   => 12,
      Point      => 1,
      Quandratic => 9,
      Sinc       => 15,
      Triangle   => 3);
   --
   --
   function Magnify_Image (I : in Image_Ptr) return Image_Ptr;
   --
   function Minify_Image (I : in Image_Ptr) return Image_Ptr;
   --
   function Resize_Image
     (I      : in Image_Ptr;
      I_S    : in G2f.Io.Image_Size;
      Filter : Resize_Filter := Lanczos;
      Blur   : T_Blur        := 1.0)
      return   Image_Ptr;
   --
   function Sample_Image
     (I    : in Image_Ptr;
      I_S  : in G2f.Io.Image_Size)
      return Image_Ptr;
   --
   function Scale_Image
     (I    : in Image_Ptr;
      I_S  : in G2f.Io.Image_Size)
      return Image_Ptr;
   --
   function Thumbnail_Image
     (I    : in Image_Ptr;
      I_S  : in G2f.Io.Image_Size)
      return Image_Ptr;

end G2f.Image_Resize;
