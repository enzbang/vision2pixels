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

with G2F.IO;

package body G2F.Image_Resize is

   -------------------
   -- Magnify_Image --
   -------------------

   function Magnify_Image (I : in Image_Ptr) return Image_Ptr is
      function C_Magnify_Image
        (I : in Image_Ptr;
         E : in Exception_Info_Ptr := Ex_Info_Ptr) return Image_Ptr;
      pragma Import (C, C_Magnify_Image, "MagnifyImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Magnify_Image (I);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Magnify_Image;

   ------------------
   -- Minify_Image --
   ------------------

   function Minify_Image (I : in Image_Ptr) return Image_Ptr is
      function C_Minify_Image
        (I : in Image_Ptr;
         E : in Exception_Info_Ptr := Ex_Info_Ptr) return Image_Ptr;
      pragma Import (C, C_Minify_Image, "MinifyImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Minify_Image (I);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Minify_Image;

   ------------------
   -- Resize_Image --
   ------------------

   function Resize_Image
     (I      : in Image_Ptr;
      I_S    : in G2F.IO.Image_Size;
      Filter : Resize_Filter := Lanczos;
      Blur   : T_Blur        := 1.0) return Image_Ptr
   is
      function C_Resize_Image
        (I           : in Image_Ptr;
         Column, Row : in G2F.IO.Image_Size_T;
         Filter      : in Resize_Filter;
         Blur        : in T_Blur;
         E           : in Exception_Info_Ptr := Ex_Info_Ptr) return Image_Ptr;
      pragma Import (C, C_Resize_Image, "ResizeImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Resize_Image (I, I_S.X, I_S.Y, Filter, Blur, Ex_Info_Ptr);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Resize_Image;

   ------------------
   -- Sample_Image --
   ------------------

   function Sample_Image
     (I   : in Image_Ptr;
      I_S : in G2F.IO.Image_Size) return Image_Ptr
   is
      function C_Sample_Image
        (I           : in Image_Ptr;
         Column, Row : in G2F.IO.Image_Size_T;
         E           : in Exception_Info_Ptr := Ex_Info_Ptr) return Image_Ptr;
      pragma Import (C, C_Sample_Image, "SampleImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Sample_Image (I, I_S.X, I_S.Y);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Sample_Image;

   -----------------
   -- Scale_Image --
   -----------------

   function Scale_Image
     (I   : in Image_Ptr;
      I_S : in G2F.IO.Image_Size) return Image_Ptr
   is
      function C_Scale_Image
        (I             : in Image_Ptr;
         Columns, Rows : in G2F.IO.Image_Size_T;
         E             : in Exception_Info_Ptr := Ex_Info_Ptr)
         return          Image_Ptr;
      pragma Import (C, C_Scale_Image, "ScaleImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Scale_Image (I, I_S.X, I_S.Y);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Scale_Image;

   ---------------------
   -- Thumbnail_Image --
   ---------------------

   function Thumbnail_Image
     (I    : in Image_Ptr;
      I_S  : in G2F.IO.Image_Size)
      return Image_Ptr
   is
      function C_Thumbnail_Image
        (I             : in Image_Ptr;
         Columns, Rows : in G2F.IO.Image_Size_T;
         E             : in Exception_Info_Ptr := Ex_Info_Ptr)
         return Image_Ptr;
      pragma Import (C, C_Thumbnail_Image, "ThumbnailImage");

      Mon_Image : Image_Ptr;
   begin
      Mon_Image := C_Thumbnail_Image (I, I_S.X, I_S.Y);
      if Mon_Image = null then
         raise Resize_Error;
      end if;
      return Mon_Image;
   end Thumbnail_Image;

end G2F.Image_Resize;
