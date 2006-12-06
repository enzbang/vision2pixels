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
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Directories;

with G2F.Image_IO;
with Image.Magick;
with Settings;

package body Image.Data is

   use Ada.Text_IO;
   use Ada.Directories;
   use G2F;
   use G2F.IO;
   use G2F.Image_IO;

   --------------
   -- Filename --
   --------------

   function Filename (Img : in Image_Data) return String is
   begin
      return Get_Filename (Img.Image_Ptr);
   end Filename;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Img : in out Image_Data) is
   begin
      if not Is_Null (Img.Image_Ptr) then
         Destroy_Image (Img.Image_Ptr);
      end if;
      if not Is_Null (Img.Info_Ptr) then
         Destroy_Image_Info (Img.Info_Ptr);
      end if;
   end Finalize;

   ------------
   -- Height --
   ------------

   function Height (Img : in Image_Data) return Integer is
   begin
      if Img.Init_Status /= Image_Created then
         raise Image_Error
           with "Get_Height : Error image not created";
      end if;
      return Img.Height;
   end Height;

   ----------
   -- Init --
   ----------

   procedure Init
     (Img      : in out Image_Data;
      Filename : in     String;
      Category : in     String)
   is
      Thumb_Name : constant String :=
                     Settings.Get_Thumbs_Path
                       & "/" & Category & "/" & Simple_Name (Filename);
      Image_Name : constant String :=
                     Settings.Get_Images_Path
                       & "/" & Category & "/" & Simple_Name (Filename);
      Thumb      : Image_Ptr;
      Thumb_Info : Image_Info_Ptr;
   begin
      if not Exists (Containing_Directory (Thumb_Name)) then
         Create_Path (Containing_Directory (Thumb_Name));
      end if;

      if not Exists (Containing_Directory (Image_Name)) then
         Create_Path (Containing_Directory (Image_Name));
      end if;


      --  Read image info

      Set_Filename (Img.Info_Ptr, Filename);
      Img.Category := To_Unbounded_String (Category);
      Img.Image_Ptr := Read_Image (Img.Info_Ptr);

      if Settings.Limit_Image_Size then
         declare
            Dimension : constant Image_Size := Get_Image_Size (Img.Image_Ptr);
         begin
            Img.Width  := Integer (Dimension.X);
            Img.Height := Integer (Dimension.Y);
            Img.Size   := Integer (Size (Filename));

            if Img.Width > Settings.Image_Maximum_Width or
              Img.Height > Settings.Image_Maximum_Height then
               Img.Init_Status := Image.Data.Exceed_Max_Image_Dimension;
               return;
            end if;

            if Img.Size > Settings.Image_Maximum_Size then
               Img.Init_Status := Image.Data.Exceed_Max_Size;
               return;
            end if;
         end;
      end if;

      --  Save Image in Images_Path/Category

      Set_Filename (Img.Image_Ptr, Image_Name);
      Write_Image (Img.Info_Ptr, Img.Image_Ptr);

      --  Create thumbnail

      Thumb_Info := Clone_Image_Info (Img.Info_Ptr);
      Thumb := Read_Image (Thumb_Info);
      Set_Filename (Thumb, Thumb_Name);
      Thumb := Magick.Thumbnail (Thumb, Thumbnail_Size);
      Write_Image (Thumb_Info, Thumb);

      Destroy_Image (Thumb);
      Destroy_Image_Info (Thumb_Info);

      Img.Init_Status := Image_Created;

   exception
      when G2F.Image_IO.Read_Image_Error =>
         Put_Line ("Read image error - Thumbnail has not been created");
   end Init;

   function Init_Status (Img : in Image_Data) return Image_Init_Status is
   begin
      return Img.Init_Status;
   end Init_Status;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Img : in out Image_Data) is
      Info : Image_Info_Ptr;
   begin
      Img.Info_Ptr := Clone_Image_Info (Info);
   end Initialize;

   ----------
   -- Size --
   ----------

   function Size (Img : in Image_Data) return Integer is
   begin
      if Img.Init_Status /= Image_Created then
         raise Image_Error
           with "Get_Size : Error image not created";
      end if;
      return Img.Size;
   end Size;

   -----------
   -- Width --
   -----------

   function Width (Img : in Image_Data) return Integer is
   begin
      if Img.Init_Status /= Image_Created then
         raise Image_Error
           with "Get_Width : Error image not created";
      end if;
      return Img.Width;
   end Width;


begin
   if not Exists (Settings.Get_Images_Path) then
      Create_Path (Settings.Get_Images_Path);
   end if;

   if not Exists (Settings.Get_Thumbs_Path) then
      Create_Path (Settings.Get_Thumbs_Path);
   end if;
end Image.Data;
