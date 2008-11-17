------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2008                          --
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

with V2P.Settings;

with Morzhol.OS;
with G2F.Image_IO;
with Image.Magick;

package body Image.Data is

   use Ada.Text_IO;
   use Ada.Directories;
   use G2F;
   use G2F.Image_IO;
   use G2F.IO;
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
        (Width         => Image_Size_T (Settings.Image_Maximum_Width),
         Height        => Image_Size_T (Settings.Image_Maximum_Height),
         Medium_Width  => Image_Size_T (Settings.Medium_Maximum_Width),
         Medium_Height => Image_Size_T (Settings.Medium_Maximum_Height),
         Thumb_Width   => Image_Size_T (Settings.Thumbnail_Maximum_Width),
         Thumb_Height  => Image_Size_T (Settings.Thumbnail_Maximum_Height),
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
      return G2F.IO.Get_Filename (Img.Image_Ptr);
   end Filename;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Img : in out Image_Data) is
   begin
      Destroy_Image (Img.Image_Ptr);
      Destroy_Image_Info (Img.Info_Ptr);
   end Finalize;

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
      Thumb_Size  : constant G2F.IO.Image_Size := Image_Size'
        (X => Image_Size_T (Settings.Thumbnail_Maximum_Width),
         Y => Image_Size_T (Settings.Thumbnail_Maximum_Height));
      Thumb, R_Thumb : Image_Ptr;
      Thumb_Info     : Image_Info_Ptr;

      Medium_Size : constant G2F.IO.Image_Size := Image_Size'
        (X => Image_Size_T (Settings.Medium_Maximum_Width),
         Y => Image_Size_T (Settings.Medium_Maximum_Height));
      Medium, R_Medium : Image_Ptr;
      Medium_Info      : Image_Info_Ptr;

   begin
      --  Read image info

      Set_Filename (Img.Info_Ptr, Original_Filename);

      Img.Image_Ptr := Read_Image (Img.Info_Ptr);

      if Settings.Limit_Image_Size
        or else Out_Max_Dimension /= Null_Dimension
      then
         Check_Size : declare
            Dim : constant Image_Size := Get_Image_Size (Img.Image_Ptr);
         begin
            Img.Dimension := Image_Dimension'
              (Width         => Dim.X,
               Height        => Dim.Y,
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

      Set_Filename (Img.Image_Ptr, Out_Filename_Big);
      Write_Image (Img.Info_Ptr, Img.Image_Ptr);

      --  Create 800x800 image

      Medium_Info := Clone_Image_Info (Img.Info_Ptr);
      Medium := Read_Image (Medium_Info);

      Set_Filename (Medium, Out_Filename_Medium);

      R_Medium := Magick.Resize (Medium, Medium_Size);
      Write_Image (Medium_Info, R_Medium);

      Set_Medium_Dimension : declare
         Dim : constant Image_Size := Get_Image_Size (R_Medium);
      begin
         Img.Dimension.Medium_Width  := Dim.X;
         Img.Dimension.Medium_Height := Dim.Y;
      end Set_Medium_Dimension;

      Destroy_Image (Medium);
      Destroy_Image (R_Medium);
      Destroy_Image_Info (Medium_Info);

      --  Create thumbnail

      Thumb_Info := Clone_Image_Info (Img.Info_Ptr);
      Thumb := Read_Image (Thumb_Info);

      Set_Filename (Thumb, Out_Filename_Thumb);

      R_Thumb := Magick.Thumbnail (Thumb, Thumb_Size);
      Write_Image (Thumb_Info, R_Thumb);

      Set_Thumb_Dimension : declare
         Dim : constant Image_Size := Get_Image_Size (R_Thumb);
      begin
         Img.Dimension.Thumb_Width  := Dim.X;
         Img.Dimension.Thumb_Height := Dim.Y;
      end Set_Thumb_Dimension;

      Destroy_Image (Thumb);
      Destroy_Image (R_Thumb);
      Destroy_Image_Info (Thumb_Info);

      Img.Init_Status := Image_Created;

   exception
      when others =>
         Put_Line
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
      S_Name          : constant String := Simple_Name (Filename);
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
   --  Initialize  --
   ------------------

   procedure Initialize (Img : in out Image_Data) is
   begin
      Img.Info_Ptr := Clone_Image_Info (null);
   end Initialize;

end Image.Data;
