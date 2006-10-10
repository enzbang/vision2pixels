------------------------------------------------------------------------------
--                              G2f_IO                                      --
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

with G2F;  use G2F;
with G2F.IO; use G2F.IO;
with G2F.Image_IO; use G2F.Image_IO;
with G2F.Image_Enhance;
with G2F.Image_Resize;
with G2F.Image_Text_Attribute;
with Ada.Text_IO;

procedure G2F_Test is

   Info : Image_Info_Ptr;
   Test_Image : Image_Ptr;
   My_Size, Lite_Size : Image_Size;
   In_Filename : constant String := "adapowered.jpg";
   Out_Filename : constant String := "magick.png";
begin

   Info := Clone_Image_Info (Info);

   --  Image Filename

   Ada.Text_IO.Put ("Filename in : ");
   Set_Filename (Info, In_Filename);
   Ada.Text_IO.Put_Line (Get_Filename (Info));

   Set_Format (Info, Magick_JPEG);
   My_Size := (200, 50);
   Set_Image_Size (Info, My_Size);
   Ada.Text_IO.Put ("Filename in : ");
   Ada.Text_IO.Put_Line (Get_Filename (Info));
   Ada.Text_IO.Put_Line (Get_Format (Info));

   Set_Depth (Info, 8);

   --  Read image

   Test_Image := Read_Image (Info);

   Ada.Text_IO.Put ("Filename out : ");
   Set_Filename (Test_Image, Out_Filename);

   Ada.Text_IO.Put_Line (Get_Filename (Test_Image));

   --  Image depth

   Ada.Text_IO.Put ("Depth in image_info:");
   Ada.Text_IO.Put_Line (Depth'Image (Get_Depth (Info)));

   Ada.Text_IO.Put ("Depth in image :");
   Ada.Text_IO.Put_Line (Depth'Image (Get_Depth (Test_Image)));
   Ada.Text_IO.Put_Line ("Setting image out depth to 16 bit");
   Set_Depth (Test_Image, 16);
   Ada.Text_IO.Put ("Depth in image :");
   Ada.Text_IO.Put_Line (Depth'Image (Get_Depth (Test_Image)));

   --  Image compression

   Ada.Text_IO.Put ("Image compression methode : ");
   Ada.Text_IO.Put_Line
     (Compression_Type'Image (Get_Compression (Test_Image)));
   Ada.Text_IO.Put ("Setting image compression to : ");


   --  You may take care to use a compression methode supported by the
   --  image format and the target system.

   Ada.Text_IO.Put_Line
     (Compression_Type'Image (Get_Compression (Test_Image)));

   Ada.Text_IO.Put_Line ("Get image size");
   My_Size := Get_Image_Size (Test_Image);

   Ada.Text_IO.Put_Line ("x : " & Image_Size_T'Image (My_Size.X));
   Ada.Text_IO.Put_Line ("y : " & Image_Size_T'Image (My_Size.Y));

   Lite_Size := (X => 400, Y => 300);

   Test_Image := G2F.Image_Resize.Resize_Image
     (Test_Image, Lite_Size, G2F.Image_Resize.Mitchell, 0.3);

   G2F.Image_Enhance.Negate_Image (Test_Image);
   G2F.Image_Text_Attribute.Set_Image_Text_Attribute
     (Test_Image,
      G2F.Image_Text_Attribute.Copyright,
      "Ali Bendriss");
   G2F.Image_Text_Attribute.Set_Image_Text_Attribute
     (Test_Image,
      G2F.Image_Text_Attribute.Software,
      "G2F_IO Version alpha 03");
   Ada.Text_IO.Put_Line ("Software: "
                           & G2F.Image_Text_Attribute.Get_Image_Text_Attribute
                           (Test_Image, G2F.Image_Text_Attribute.Software));
   Ada.Text_IO.Put_Line ("Copyright: "
                           & G2F.Image_Text_Attribute.Get_Image_Text_Attribute
                           (Test_Image, "Copyright"));

   --
   --  Write Image
   --

   Set_Format (Test_Image, Magick_PNG);
   Ada.Text_IO.Put ("My_Image : ");
   Ada.Text_IO.Put_Line (Get_Format (Test_Image));
   Write_Image (Info, Test_Image);

   Ada.Text_IO.Put_Line ("At this stage write image is done !");

   --
   --  Finish
   --

   Destroy_Image (Test_Image);
   Destroy_Image_Info (Info);
   Destroy_Magick;

exception
   when G2F.Image_IO.Write_Image_Error =>
      Put_Image_Exception (Test_Image);
   when G2F.Image_IO.Read_Image_Error =>
      Put_Magick_Exception;
      raise;
   when G2F.Image_Resize.Resize_Error =>
      Put_Magick_Exception;
      raise;
end G2F_Test;
