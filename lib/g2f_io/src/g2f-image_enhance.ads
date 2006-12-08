------------------------------------------------------------------------------
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

package G2F.Image_Enhance is

   Enhance_Error : exception;

   type Sharpen_T is (Increase, Decrease);
   subtype Max_Rgb is Quantum_Depth;

   type Gamma_Parameter is digits 1 range 0.0 .. 10.0;
   type Persent_Level is new Natural range 0 .. 100;

   type Channel_T is
     (Red,
      Cyan,
      Green,
      Magenta,
      Blue,
      Yellow,
      Opacity);
   for Channel_T use (1, 2, 3, 4, 5, 6, 7);

   procedure Contrast_Image
     (I       : in out Image_Ptr;
      Sharpen : in Sharpen_T;
      Factor  : in Positive := 1);
   --  Enhances the intensity differences between the lighter and darker
   --  elements of the image. Set sharpen to a MagickTrue to increase the image
   --  contrast otherwise the contrast is reduced.

   procedure Equalize_Image (I : in out Image_Ptr);
   --  Applies a histogram equalization to the image.

   procedure Gamma_Image
     (I                : in out Image_Ptr;
      Red, Green, Blue : in Gamma_Parameter := 1.0);
   --  Gamma-corrects a particular image channel. The same image viewed on
   --  different devices will have perceptual differences in the way the
   --  image's intensities are represented on the screen. Specify individual
   --  gamma levels for the red, green, and blue channels, or adjust all three
   --  with the gamma parameter. Values typically range from 0.8 to 2.3.

   procedure Level_Image
     (I                            : in out Image_Ptr;
      Black_Percent, White_Percent : in Persent_Level;
      Gamma                        : in Gamma_Parameter := 1.0);
   --  Adjusts the levels of a particular image channel by scaling the colors
   --  falling between specified white and black points to the full available
   --  quantum range. The parameters provided represent the black, mid, and
   --  white points. The black point specifies the darkest color in the image.
   --  Colors darker than the black point are set to zero. Gamma specifies a
   --  gamma correction to apply to the image. White point specifies the
   --  lightest color in the image. Colors brighter than the white point are
   --  set to the maximum quantum value.

   procedure Level_Image_Channel
     (I                        : in out Image_Ptr;
      Channel                  : in Channel_T;
      Black_Point, White_Point : in Max_Rgb;
      Gamma                    : in Gamma_Parameter := 1.0);
   --  Adjusts the levels of a particular image channel by scaling the colors
   --  falling between specified white and black points to the full available
   --  quantum range. The parameters provided represent the black, mid, and
   --  white points. The black point specifies the darkest color in the image.
   --  Colors darker than the black point are set to zero. Gamma specifies a
   --  gamma correction to apply to the image. White point specifies the
   --  lightest color in the image. Colors brighter than the white point are
   --  set to the maximum quantum value.

   procedure Modulate_Image
     (I                           : in out Image_Ptr;
      Brightness, Saturation, Hue : in Persent_Level);
   --  Lets you control the brightness, saturation, and hue of an image.
   --  Modulate represents the brightness, saturation, and hue as one parameter
   --  ( e.g. 90, 150, 100 ) .

   procedure Negate_Image
     (I              : in out Image_Ptr;
      Only_Grayscale : in Boolean := False);
   --  Negates the colors in the reference image. The Grayscale option means
   --  that only grayscale values within the image are negated.

   procedure Normalize_Image (I : in out Image_Ptr);
   --  Enhances the contrast of a color image by adjusting the pixels color to
   --  span the entire range of colors available.

end G2F.Image_Enhance;
