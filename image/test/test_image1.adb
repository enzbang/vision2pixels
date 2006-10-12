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

pragma Warnings (Off);

with Ada.Text_IO;
with G2F.Image_Resize;
with G2F.Image_IO;
with G2F.IO;
with Image.Magick;

procedure Test_Image1 is

   use G2F;
   use G2F.Image_Resize;
   use G2F.Image_IO;
   use G2F.IO;

   Info : Image_Info_Ptr;
   Test_Image : Image_Ptr;
   In_Filename : constant String := "adapowered.jpg";
   Out_Filename : constant String := "adapowsered_thumb.jpg";
   Size_1 : constant Image_Size := (150, 150);

begin
   Info := Clone_Image_Info (Info);
   Ada.Text_IO.Put ("Begin test...");

   Set_Filename (Info, In_Filename);
   Set_Format (Info, Magick_JPEG);

   --  Set_Depth (Info, 8);
   Test_Image := Read_Image (Info);
   Set_Filename (Test_Image, Out_Filename);
   Test_Image := Image.Magick.Thumbnail (Test_Image, Size_1);
   Write_Image (Info, Test_Image);

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
end Test_Image1;
