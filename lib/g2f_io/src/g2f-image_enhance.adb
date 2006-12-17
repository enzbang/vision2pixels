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

with Ada.Text_IO;

package body G2F.Image_Enhance is

   --------------------
   -- Contrast_Image --
   --------------------

   procedure Contrast_Image
     (I       : in out Image_Ptr;
      Sharpen : in Sharpen_T;
      Factor  : in Positive := 1)
   is
      use C;

      function C_Contrast_Image
        (I     : in Image_Ptr;
         Sharp : in C.unsigned) return  C.int;
      pragma Import (C, C_Contrast_Image, "ContrastImage");

      Res : C.int      := 0;
      S   : C.unsigned := 0;
   begin
      if Sharpen = Increase then
         S := 1;
      end if;

      for Count in  0 .. Factor loop
         Res := C_Contrast_Image (I, S);
      end loop;

      -- in ImageMagick-5.7 enhance.c the function return false!
      -- if Res = 0 or I.all.Image_Exception.Severity /= 0 then
      if Res /= 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Contrast_Image;

   --------------------
   -- Equalize_Image --
   --------------------

   procedure Equalize_Image (I : in out Image_Ptr) is
      use C;
      function C_Equalize_Image (I : in Image_Ptr) return C.int;
      pragma Import (C, C_Equalize_Image, "EqualizeImage");

      Res : C.int := 0;
   begin
      Res := C_Equalize_Image (I);
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Equalize_Image;

   -----------------
   -- Gamma_Image --
   -----------------

   procedure Gamma_Image
     (I                : in out Image_Ptr;
      Red, Green, Blue : in Gamma_Parameter := 1.0)
   is
      use C;
      function C_Gamma_Image
        (I : in Image_Ptr;
         G : in C.char_array) return C.int;
      pragma Import (C, C_Gamma_Image, "GammaImage");

      Res : C.int := 0;
   begin
      Res :=
         C_Gamma_Image
           (I,
            To_C
               (Gamma_Parameter'Image (Red) &
                '/' &
                Gamma_Parameter'Image (Green) &
                '/' &
                Gamma_Parameter'Image (Blue)));
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Gamma_Image;

   -----------------
   -- Level_Image --
   -----------------

   procedure Level_Image
     (I                            : in out Image_Ptr;
      Black_Percent, White_Percent : in Persent_Level;
      Gamma                        : in Gamma_Parameter := 1.0)
   is
      use C;
      function C_Level_Image
        (I     : in Image_Ptr;
         Level : in C.char_array) return  C.int;
      pragma Import (C, C_Level_Image, "LevelImage");

      Res : C.int := 0;
   begin
      Res :=
         C_Level_Image
           (I,
            To_C
               (Persent_Level'Image (Black_Percent) &
                "%" &
                ',' &
                Persent_Level'Image (White_Percent) &
                "%" &
                ',' &
                Gamma_Parameter'Image (Gamma)));
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Level_Image;

   -------------------------
   -- Level_Image_Channel --
   -------------------------

   procedure Level_Image_Channel
     (I                        : in out Image_Ptr;
      Channel                  : in Channel_T;
      Black_Point, White_Point : in Max_Rgb;
      Gamma                    : in Gamma_Parameter := 1.0)
   is
      use C;
      function C_Level_Image_Channel
        (I        : in Image_Ptr;
         Channel  : Channel_T;
         B_P, W_P : in Max_Rgb;
         G        : in Gamma_Parameter) return C.int;
      pragma Import (C, C_Level_Image_Channel, "LevelImageChannel");

      Res : C.int := 0;
   begin
      Res :=
         C_Level_Image_Channel (I, Channel, Black_Point, White_Point, Gamma);
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Level_Image_Channel;

   --------------------
   -- Modulate_Image --
   --------------------

   procedure Modulate_Image
     (I                           : in out Image_Ptr;
      Brightness, Saturation, Hue : in Persent_Level)
   is
      use C;
      function C_Modulate_Image
        (I     : in Image_Ptr;
         Level : in C.char_array) return C.int;
      pragma Import (C, C_Modulate_Image, "ModulateImage");

      Res : C.int := 0;
   begin
      Res :=
         C_Modulate_Image
           (I,
            To_C
               (Persent_Level'Image (Brightness) &
                ',' &
                Persent_Level'Image (Saturation) &
                ',' &
                Persent_Level'Image (Hue)));
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Modulate_Image;

   ------------------
   -- Negate_Image --
   ------------------

   procedure Negate_Image
     (I              : in out Image_Ptr;
      Only_Grayscale : in Boolean := False)
   is
      use C;
      function C_Negate_Image
        (I   : in Image_Ptr;
         O_G : in C.unsigned) return C.int;
      pragma Import (C, C_Negate_Image, "NegateImage");

      Res : C.int := 0;
   begin
      if not Only_Grayscale then
         Res := C_Negate_Image (I, 0);
      else
         Res := C_Negate_Image (I, 1);
      end if;
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Negate_Image;

   ---------------------
   -- Normalize_Image --
   ---------------------

   procedure Normalize_Image (I : in out Image_Ptr) is
      use C;
      function C_Normalize_Image (I : in Image_Ptr) return C.int;
      pragma Import (C, C_Normalize_Image, "NormalizeImage");

      Res : C.int := 0;
   begin
      Res := C_Normalize_Image (I);
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Enhance_Error;
      end if;
   end Normalize_Image;

end G2F.Image_Enhance;
