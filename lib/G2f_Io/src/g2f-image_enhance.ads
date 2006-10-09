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

with G2f; use G2f;
package G2f.Image_Enhance is

   Enhance_Error:exception;

   type Sharpen_T is (Increase,Decrease);
   subtype Max_Rgb is Quantum_Depth;
   --type Gamma_Parameter is digits 1 range 0.1..3.0;
   type Gamma_Parameter is digits 1 range 0.0..10.0;
   type Persent_Level is new Natural range 0..100;

   type Channel_T is (Red, Cyan, Green, Magenta, Blue, Yellow, Opacity);
   for Channel_T use (1,2,3,4,5,6,7);
   --
   --
   --
   procedure Contrast_Image (I: in out Image_Ptr; Sharpen:in Sharpen_T; Factor:in Positive:=1);
   --
   --
   procedure Equalize_Image (I:in out Image_Ptr);
   --
   --
   procedure Gamma_Image (I:in out Image_Ptr; Red,Green,Blue:in Gamma_Parameter:=1.0);
   --
   --
   procedure Level_Image (I:in out Image_Ptr; Black_Percent,White_Percent:in Persent_Level;Gamma:in Gamma_Parameter:=1.0);
   --
   --
   procedure Level_Image_Channel (I:in out Image_Ptr; Channel:in Channel_T;
                                                      Black_Point,White_Point:in Max_Rgb;
                                                      Gamma:in Gamma_Parameter:=1.0);
   --
   --
   procedure Modulate_Image (I:in out Image_Ptr; Brightness,Saturation,Hue:in Persent_Level);
   --
   --
   procedure Negate_Image (I:in out Image_Ptr; Only_Grayscale: in Boolean:=False);
   --
   --
   procedure Normalize_Image(I:in out Image_Ptr);
   --
   --
end G2f.Image_Enhance;
