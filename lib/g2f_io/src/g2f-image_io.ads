-------------------------------------------------------------------------------
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

with G2F;

package G2F.Image_IO is

   Clone_ImageInfo_Error, Read_Image_Error,
   Write_Image_Error, Ping_Image_Error : exception;

   function Clone_Image_Info
     (Image_Info_In : in Image_Info_Ptr) return Image_Info_Ptr;
   --  Makes a duplicate of the given image info, or if image info is NULL, a
   --  new one.

   procedure Destroy_Image_Info (Image_Info_In : in out Image_Info_Ptr);
   --  Deallocates memory associated with an ImageInfo structure.

   function Read_Image (I : in Image_Info_Ptr) return Image_Ptr;
   --  Reads an image and returns it. It allocates the memory necessary for the
   --  new Image structure and returns a pointer to the new image. By default,
   --  the image format is determined by its magic number. To specify a
   --  particular image format, precede the filename with an explicit image
   --  format name and a colon (i.e. ps:image) or as the filename suffix (i.e.
   --  image.ps).

   procedure Write_Image (I : in Image_Info_Ptr; E : in Image_Ptr);
   --  Writes an image to a file as defined by image->filename. You can specify
   --  a particular image format by prefixing the file with the image type and
   --  a colon (i.e. ps:image) or specify the image type as the filename suffix
   --  (i.e. image.ps). The image may be modified to adapt it to the
   --  requirements of the image format. For example, DirectClass images must
   --  be color-reduced to PseudoClass if the format is GIF.

   procedure Destroy_Image (Image_In : in out Image_Ptr);
   --  Deallocates memory associated with an image.

   function Ping_Image (I : in Image_Info_Ptr) return Image_Ptr;
   --  Returns the image size in bytes if it exists and can be the image is
   --  returned as well. Note, only the first image in a multi-frame image file
   --  is pinged.

end G2F.Image_IO;
