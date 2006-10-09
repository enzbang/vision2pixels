---------------------------------------------------------
-----------------------
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

with Ada.Text_IO; use Ada.Text_IO;

with G2f;
with G2f.Io; use G2f.Io;

package body G2f.Image_Io is

   -- image_info
   function Clone_Image_Info
     (Image_Info_In : in Image_Info_Ptr)
      return          Image_Info_Ptr
   is
      function C_CloneImageInfo
        (Image_Info_In : in Image_Info_Ptr)
         return          Image_Info_Ptr;
      pragma Import (C, C_CloneImageInfo, "CloneImageInfo");
      Info : Image_Info_Ptr := null;
   begin
      Info := C_CloneImageInfo (Image_Info_In);
      if Info = null then
         raise Clone_ImageInfo_Error;
      end if;
      return Info;
   end Clone_Image_Info;

   procedure Destroy_Image_Info (Image_Info_In : in out Image_Info_Ptr) is
      procedure C_DestroyImageInfo (Image_Info_In : in Image_Info_Ptr);
      pragma Import (C, C_DestroyImageInfo, "DestroyImageInfo");
   begin
      C_DestroyImageInfo (Image_Info_In);
      --      Free(Image_Info_In);
   end Destroy_Image_Info;

   -- /image_info

   -- image

   function Read_Image (I : in Image_Info_Ptr) return Image_Ptr is
      function C_Read_Image
        (I    : in Image_Info_Ptr;
         E    : in Exception_Info_Ptr := Ex_Info_Ptr)
         return Image_Ptr;
      pragma Import (C, C_Read_Image, "ReadImage");
      Mon_Image : Image_Ptr := null;
   begin
      Put_Line ("Size ImageInfo = " & Integer'Image (I.all'Size / 8));
      Put_Line
        ("Size PixelPacket = " & Integer'Image (Pixel_Packet'Size / 8));
      Mon_Image := C_Read_Image (I);
      if Mon_Image = null then
         raise Read_Image_Error;
      end if;
      Set_Compression (Mon_Image, NoCompression);  --the user may explicitly
                                                   --give a compression
                                                   --methode.
      return Mon_Image;
   end Read_Image;

   procedure Write_Image (I : in Image_Info_Ptr; E : in Image_Ptr) is
      use Interfaces.C;
      -- use interfaces.c.Strings;
      function C_Write_Image
        (I    : in Image_Info_Ptr;
         E    : in Image_Ptr)
         return unsigned;
      pragma Import (C, C_Write_Image, "WriteImage");
      Res : unsigned := 0;
   begin
      Res := C_Write_Image (I, E);
      --if Res = 0 or E.all.Image_Exception.Reason /=
      --interfaces.c.strings.Null_Ptr then
      if Res = 0 or E.all.Image_Exception.Severity /= 0 then
         raise Write_Image_Error;
      end if;
   end Write_Image;

   function Ping_Image (I : in Image_Info_Ptr) return Image_Ptr is
      function C_Ping_Image
        (I    : in Image_Info_Ptr;
         E    : in Exception_Info_Ptr := Ex_Info_Ptr)
         return Image_Ptr;
      pragma Import (C, C_Ping_Image, "PingImage");
      Ping_Image : Image_Ptr := null;
   begin
      Ping_Image := C_Ping_Image (I);
      if Ping_Image = null then
         raise Ping_Image_Error;
      end if;
      return Ping_Image;
   end Ping_Image;

   procedure Destroy_Image (Image_In : in out Image_Ptr) is
      procedure C_DestroyImage (Image_In : in Image_Ptr);
      pragma Import (C, C_DestroyImage, "DestroyImage");
   begin
      C_DestroyImage (Image_In);
      --      Free(Image_In);
   end Destroy_Image;
   -- /image

end G2f.Image_Io;
