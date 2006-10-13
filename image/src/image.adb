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

with G2F.Image_IO;
with Image.Magick;
with Ada.Text_IO;

package body Image is

   use G2F;
   use G2F.IO;
   use G2F.Image_IO;
   use Ada.Text_IO;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Img : in Image_Data) is
      Info_Ptr : Image_Info_Ptr := Img.Info_Ptr;
      Img_Ptr  : Image_Ptr      := Img.Image_Ptr;
   begin
      Destroy_Image (Img_Ptr);
      Destroy_Image_Info (Info_Ptr);
   end Finalize;

   -----------------------
   --  Init_Image_Data  --
   -----------------------

   procedure Init_Image_Data
     (Img : in out Image_Data; Filename : in String) is
      Thumb : Image_Ptr;
      Thumb_Info : Image_Info_Ptr;
      Thumb_Name : constant String := "thumb_" & Filename;
   begin

      --  Read image info

      Set_Filename (Img.Info_Ptr, Filename);
      Img.Image_Ptr := Read_Image (Img.Info_Ptr);

      --  Create thumbnail

      Thumb_Info := Clone_Image_Info (Img.Info_Ptr);
      Thumb := Read_Image (Thumb_Info);
      Set_Filename (Thumb, Thumb_Name);
      Thumb := Magick.Thumbnail (Thumb, Thumbnail_Size);
      Write_Image (Thumb_Info, Thumb);

      Destroy_Image (Thumb);
      Destroy_Image_Info (Thumb_Info);

   exception
      when G2F.Image_IO.Read_Image_Error =>
         Put_Line ("Read image error - Thumbnail has not been created");
   end Init_Image_Data;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Img : in out Image_Data) is
      Info : Image_Info_Ptr;
   begin
      Img.Info_Ptr := Clone_Image_Info (Info);
   end Initialize;

end Image;
