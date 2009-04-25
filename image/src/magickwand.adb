------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with Interfaces.C.Strings;

package body MagickWand is

   use Interfaces.C;
   use Interfaces.C.Strings;

   -----------
   -- Clone --
   -----------

   procedure Clone (New_Object : in out Object; From : in Object) is
      function CloneMagickWand
        (Wand : in System.Address) return System.Address;
      pragma Import (C, CloneMagickWand, "CloneMagickWand");
      --  wand/MagickWand.h:182:4
   begin
      New_Object.Wand := CloneMagickWand (From.Wand);
   end Clone;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (O : in out Object) is
      function DestroyMagickWand
        (Wand : in System.Address) return System.Address;
      pragma Import (C, DestroyMagickWand, "DestroyMagickWand");
      --  wand/MagickWand.h:183:4

      use type System.Address;
   begin
      O.Wand := DestroyMagickWand (O.Wand);
   end Finalize;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (O : in Object) return String is
      function MagickGetImageFilename
        (Wand : in System.Address) return Interfaces.C.Strings.chars_ptr;
      --  wand/magick-image.h:264:3
      pragma Import (C, MagickGetImageFilename, "MagickGetImageFilename");

      C_Filename : chars_ptr       := MagickGetImageFilename (O.Wand);
      Filename   : constant String := Strings.Value (C_Filename);
   begin
      Free (C_Filename);
      return Filename;
   end Get_Filename;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (O : in Object) return Size is
      function MagickGetImageHeight (Wand : in System.Address) return Size;
      pragma Import (C, MagickGetImageHeight, "MagickGetImageHeight");
      --  wand/magick-image.h:386:3
   begin
      return MagickGetImageHeight (O.Wand);
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (O : in Object) return Size is
      function MagickGetImageWidth (Wand : in System.Address) return Size;
      pragma Import (C, MagickGetImageWidth, "MagickGetImageWidth");
      --  wand/magick-image.h:390:3
   begin
      return MagickGetImageWidth (O.Wand);
   end Get_Width;

   ----------
   -- Read --
   ----------

   procedure Read (O : in out Object; Filename : in String) is

      function MagickReadImage
        (Wand : in System.Address; Filename : in chars_ptr)
         return MagickBooleanType;
      pragma Import (C, MagickReadImage, "MagickReadImage");
      --  wand/magick-image.h:224:3

      function NewMagickWand return  System.Address;
      pragma Import (C, NewMagickWand, "NewMagickWand");
      --  wand/MagickWand.h:184:4

      C_Filename : chars_ptr := New_String (Filename);

   begin
      O.Wand := NewMagickWand;

      if MagickReadImage (O.Wand, C_Filename) = MagickFalse then
         Free (C_Filename);
         raise MagickError;
      end if;
      Free (C_Filename);
   end Read;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (O      : in Object;
      Width  : in Size;
      Height : in Size;
      Filter : in FilterTypes;
      Blur   : in Long_Float := 1.0)
   is
      function MagickResizeImage
        (Wand   : in System.Address;
         Width  : in Size;
         Height : in Size;
         Filter : in FilterTypes;
         Blur   : in double)
         return MagickBooleanType;
      pragma Import (C, MagickResizeImage, "MagickResizeImage");
      --  wand/magick-image.h:233:3
   begin
      if MagickResizeImage
        (O.Wand, Width, Height, Filter, double (Blur)) /= MagickTrue
      then
         raise MagickError with "can not resize image";
      end if;
   end Resize;

   -----------------------------
   -- Set_Compression_Quality --
   -----------------------------

   procedure Set_Compression_Quality
     (O : in Object; Value : in Float := 100.0)
   is

      function MagickSetImageCompressionQuality
        (Wand    : in System.Address;
         Quality : in unsigned_long) return MagickBooleanType;
      pragma Import
        (C,
         MagickSetImageCompressionQuality,
         "MagickSetImageCompressionQuality");
      --  wand/magick-image.h:260:3
   begin
      if MagickSetImageCompressionQuality
        (O.Wand, unsigned_long (Value)) /= MagickTrue
      then
         raise MagickError with "Can not set compression quality";
      end if;
   end Set_Compression_Quality;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename (O : in Object; Filename : in String) is

      function MagickSetImageFilename
        (Wand     : in System.Address;
         Filename : in Interfaces.C.Strings.chars_ptr)
         return MagickBooleanType;
      --  wand/magick-image.h:264:3
      pragma Import (C, MagickSetImageFilename, "MagickSetImageFilename");

      C_Filename : chars_ptr := New_String (Filename);
   begin
      if MagickSetImageFilename (O.Wand, C_Filename) /= MagickTrue then
         Free (C_Filename);
         raise MagickError with "can not set filename " & Filename;
      end if;
      Free (C_Filename);
   end Set_Filename;

   -----------
   -- Write --
   -----------

   procedure Write (O : in Object; Filename : in String) is

      function MagickWriteImage
        (Wand     : in System.Address;
         Filename : in Interfaces.C.Strings.chars_ptr)
         return MagickBooleanType;
      pragma Import (C, MagickWriteImage, "MagickWriteImage");
      --  wand/magick-image.h:326:3

      C_Filename : chars_ptr := New_String (Filename);
   begin
      if MagickWriteImage (O.Wand, C_Filename) /= MagickTrue then
         Free (C_Filename);
         raise MagickError with "write error";
      end if;
      Free (C_Filename);
   end Write;

end MagickWand;
