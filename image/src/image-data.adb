------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2010                          --
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

with Ada.Calendar;
with Ada.Text_IO;

with GNAT.Calendar.Time_IO;
with GNAT.MD5;

with V2P.Settings;

with Morzhol.OS;
with Image.Magick;

package body Image.Data is

   use Ada.Directories;
   use V2P;

   procedure Init
     (Img                 : in out Image_Data;
      Original_Filename   : in     String;
      Out_Filename_Big    : in     String;
      Out_Filename_Medium : in     String;
      Out_Filename_Thumb  : in     String;
      Out_Max_Dimension   : in     Image_Dimension := Null_Dimension);
   --  Set image filename, read image info and create thumbnail

   ---------------------------
   -- Default_Max_Dimension --
   ---------------------------

   function Default_Max_Dimension return Image_Dimension is
   begin
      return Image_Dimension'
        (Width         => MagickWand.Size
           (Settings.Image_Maximum_Width),
         Height        => MagickWand.Size
           (Settings.Image_Maximum_Height),
         Medium_Width  => MagickWand.Size
           (Settings.Medium_Maximum_Width),
         Medium_Height => MagickWand.Size
           (Settings.Medium_Maximum_Height),
         Thumb_Width   => MagickWand.Size
           (Settings.Thumbnail_Maximum_Width),
         Thumb_Height  => MagickWand.Size
           (Settings.Thumbnail_Maximum_Height),
         Size          => File_Size (Settings.Image_Maximum_Size));
   end Default_Max_Dimension;

   ---------------
   -- Dimension --
   ---------------

   function Dimension (Img : in Image_Data) return Image_Dimension is
   begin
      if Img.Init_Status /= Image_Created then
         raise Image_Error with "Get_Height : Error image not created";
      end if;
      return Img.Dimension;
   end Dimension;

   --------------
   -- Filename --
   --------------

   function Filename (Img : in Image_Data) return String is
   begin
      return MagickWand.Get_Filename (Img.Image);
   end Filename;

   ----------
   -- Init --
   ----------

   procedure Init
     (Img                 : in out Image_Data;
      Original_Filename   : in     String;
      Out_Filename_Big    : in     String;
      Out_Filename_Medium : in     String;
      Out_Filename_Thumb  : in     String;
      Out_Max_Dimension   : in     Image_Dimension := Null_Dimension)
   is
      Thumb_Size  : constant MagickWand.Image_Size :=
                      MagickWand.Image_Size'
                        (Width  => MagickWand.Size
                           (Settings.Thumbnail_Maximum_Width),
                         Height => MagickWand.Size
                           (Settings.Thumbnail_Maximum_Height));

      Medium_Size    : constant MagickWand.Image_Size :=
                         MagickWand.Image_Size'
                           (Width  => MagickWand.Size
                              (Settings.Medium_Maximum_Width),
                            Height => MagickWand.Size
                              (Settings.Medium_Maximum_Height));
      Thumb, Medium : MagickWand.Object;

   begin
      --  Read image info
      MagickWand.Read (Img.Image, Filename => Original_Filename);

      if Settings.Limit_Image_Size
        or else Out_Max_Dimension /= Null_Dimension
      then
         Check_Size : declare
            Dim : constant MagickWand.Image_Size :=
                    MagickWand.Image_Size'(Width  => Img.Image.Get_Width,
                                           Height => Img.Image.Get_Height);
         begin
            Img.Dimension := Image_Dimension'
              (Width         => Dim.Width,
               Height        => Dim.Height,
               Medium_Width  => 0,
               Medium_Height => 0,
               Thumb_Width   => 0,
               Thumb_Height  => 0,
               Size          => Size (Original_Filename));

            if Natural (Img.Dimension.Width) >  Settings.Image_Maximum_Width
              or else
                Natural (Img.Dimension.Height) > Settings.Image_Maximum_Height
            then
               Img.Init_Status := Image.Data.Exceed_Max_Image_Dimension;
               return;
            end if;

            if Natural (Img.Dimension.Size) > Settings.Image_Maximum_Size then
               Img.Init_Status := Image.Data.Exceed_Max_Size;
               return;
            end if;
         end Check_Size;
      end if;

      --  Save Image in Images_Path/Category

      Img.Image.Write (Out_Filename_Big);

      --  Create 800x800 image

      Image.Magick.Resize (Medium, Img => Img.Image, Size => Medium_Size);

      Medium.Write (Out_Filename_Medium);

      Img.Dimension.Medium_Width  := Medium.Get_Width;
      Img.Dimension.Medium_Height := Medium.Get_Height;

      --  Create thumbnail

      Image.Magick.Thumbnail (Thumb, Img => Img.Image, Size => Thumb_Size);

      Thumb.Write (Out_Filename_Thumb);

      Img.Dimension.Thumb_Width  := Thumb.Get_Width;
      Img.Dimension.Thumb_Height := Thumb.Get_Height;

      Img.Init_Status := Image_Created;

   exception
      when MagickWand.MagickError =>
         Ada.Text_IO.Put_Line
           ("Read image error - Thumbnail has not been created for "
              & Original_Filename);
   end Init;

   ----------
   -- Init --
   ----------

   procedure Init
     (Img      : in out Image_Data;
      Root_Dir : in     String;
      Filename : in     String)
   is
      Now             : constant Calendar.Time := Calendar.Clock;
      Year            : constant String :=
                          GNAT.Calendar.Time_IO.Image (Now, "%Y");
      Filename_Prefix : constant String :=
                          GNAT.Calendar.Time_IO.Image (Now, "%Y%m%d%H%M-");
      S_Name          : constant String :=
                          Compose (Name      => GNAT.MD5.Digest (Filename),
                                   Extension => Extension (Filename));
      Thumb_Name      : constant String := Compose
        (Containing_Directory => Compose
           (Containing_Directory =>
              Morzhol.OS.Compose (Root_Dir, Settings.Get_Thumbs_Path),
            Name                 => Year),
         Name                 => Filename_Prefix & S_Name);
      Image_Name      : constant String := Compose
        (Containing_Directory => Compose
           (Containing_Directory =>
              Morzhol.OS.Compose (Root_Dir, Settings.Get_Big_Images_Path),
            Name                 => Year),
         Name                 => Filename_Prefix & S_Name);
      Medium_Image_Name : constant String := Compose
        (Containing_Directory => Compose
           (Containing_Directory =>
              Morzhol.OS.Compose (Root_Dir, Settings.Get_Medium_Images_Path),
            Name                 => Year),
         Name                 => Filename_Prefix & S_Name);

   begin
      if not Exists (Containing_Directory (Thumb_Name)) then
         Create_Path (Containing_Directory (Thumb_Name));
      end if;

      if not Exists (Containing_Directory (Image_Name)) then
         Create_Path (Containing_Directory (Image_Name));
      end if;

      if not Exists (Containing_Directory (Medium_Image_Name)) then
         Create_Path (Containing_Directory (Medium_Image_Name));
      end if;

      Init (Img                 => Img,
            Original_Filename   => Filename,
            Out_Filename_Big    => Image_Name,
            Out_Filename_Medium => Medium_Image_Name,
            Out_Filename_Thumb  => Thumb_Name);
   end Init;

   -----------------
   -- Init_Status --
   -----------------

   function Init_Status (Img : in Image_Data) return Image_Init_Status is
   begin
      return Img.Init_Status;
   end Init_Status;

   ------------------
   -- Store_Avatar --
   ------------------

   procedure Store_Avatar
     (Img      : in out Image_Data;
      Root_Dir : in     String;
      Filename : in     String)
   is
      S_Name      : constant String := Simple_Name (Filename);
      Avatar_Name : constant String := Compose
        (Containing_Directory =>
           Morzhol.OS.Compose (Root_Dir, Settings.Get_Avatars_Path),
         Name                 => S_Name);
      Avatar_Size : constant MagickWand.Image_Size :=
                      MagickWand.Image_Size'
                        (Width  => MagickWand.Size
                           (Settings.Avatar_Maximum_Size),
                         Height => MagickWand.Size
                           (Settings.Avatar_Maximum_Size));
      Avatar      : MagickWand.Object;
   begin
      if not Exists (Containing_Directory (Avatar_Name)) then
         Create_Path (Containing_Directory (Avatar_Name));
      end if;

      MagickWand.Read (Img.Image, Filename => Filename);

      --  Resize to max avatar size

      Image.Magick.Resize (Avatar, Img => Img.Image, Size => Avatar_Size);

      Avatar.Write (Avatar_Name);

      Img.Init_Status := Image_Created;
   end Store_Avatar;

end Image.Data;
