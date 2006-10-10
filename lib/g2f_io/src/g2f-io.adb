--------------------------------------------------
------------------------------
--                              G2F_IO                                      --
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
with Interfaces.C.Strings;
with Ada.Strings.Fixed;

package body G2F.IO is

   use Interfaces.C;

   procedure Set_Filename (I : in Image_Info_Ptr; S : in String) is
      use Interfaces.C;
   begin
      I.all.Filename (1 .. size_t (S'Last + 1))  := To_C (S);
   end Set_Filename;

   procedure Set_Filename (I : in Image_Ptr; S : in String) is
      use Interfaces.C;
   begin
      I.all.Filename (1 .. size_t (S'Last + 1))  := To_C (S);
   end Set_Filename;

   function Get_Filename (I : in Image_Info_Ptr) return String is
      use Interfaces.C;
      Res    : String  := To_Ada (I.all.Filename);
      Count  : Natural := 0;
      Prefix : Boolean := False;
   begin
      --return To_Ada(I.all.Filename);
      for I in  Res'Range loop
         Count := Count + 1;
         if Res (I) = ':' then
            Prefix := True;
            exit;
         end if;
      end loop;
      if Prefix = True then
         Ada.Strings.Fixed.Delete (Res, 1, Count);
      end if;
      return Res;
   end Get_Filename;

   function Get_Filename (I : in Image_Ptr) return String is
      use Interfaces.C;
   begin
      return To_Ada (I.all.Filename);
   end Get_Filename;

   function To_Magick_Format
     (Format : in Supported_Image_Formats)
      return   String
   is
      use Ada.Strings.Fixed;
      Res_Tmp : String := Supported_Image_Formats'Image (Format);
   begin
      Ada.Strings.Fixed.Replace_Slice (Res_Tmp, 1, 7, "");
      return Trim (Res_Tmp, Ada.Strings.Right);
   end To_Magick_Format;

   procedure Set_Format
     (I      : in Image_Info_Ptr;
      Format : in Supported_Image_Formats)
   is
      Res    : String  := To_Magick_Format (Format);
      Name   : String  := To_Ada (I.all.Filename);
      Prefix : Boolean := False;
      Suffix : Boolean := False;
   begin
      for I in  Name'Range loop
         if Name (I) = ':' then
            Prefix := True;
         elsif Name (I) = '.' then
            Suffix := True;
         end if;
      end loop;
      if Prefix = False and then Suffix = True then
         I.all.Filename (
            size_t (Res'First) .. size_t (Res'Last + 1 + Name'Last + 1)) :=
            To_C (Res & ':' & Name);
         I.all.Magick (size_t (Res'First) .. size_t (Res'Last + 1)) :=
            To_C (Res);
      else
         I.all.Magick (size_t (Res'First) .. size_t (Res'Last + 1))  :=
            To_C (Res);
      end if;
   end Set_Format;

   procedure Set_Format
     (I      : in Image_Ptr;
      Format : in Supported_Image_Formats)
   is
      Res : String := To_Magick_Format (Format);
   begin
      I.all.Magick (size_t (Res'First) .. size_t (Res'Last + 1))  :=
         To_C (Res);
   end Set_Format;

   function Get_Format (I : in Image_Ptr) return String is
   begin
      return To_Ada (I.all.Magick);
   end Get_Format;

   function Get_Format (I : in Image_Info_Ptr) return String is
   begin
      return To_Ada (I.all.Magick);
   end Get_Format;

   procedure Set_Compression
     (I : in Image_Info_Ptr;
      C : in Compression_Type)
   is
   begin
      I.all.Compression := C;
   end Set_Compression;

   procedure Set_Compression (I : in Image_Ptr; C : in Compression_Type) is
   begin
      I.all.Compression := C;
   end Set_Compression;

   function Get_Compression (I : in Image_Info_Ptr) return Compression_Type is
   begin
      return I.all.Compression;
   end Get_Compression;

   function Get_Compression (I : in Image_Ptr) return Compression_Type is
   begin
      return I.all.Compression;
   end Get_Compression;

   procedure Set_Depth (I : in Image_Info_Ptr; D : in Depth) is
   begin
      I.all.Depth := Interfaces.C.unsigned_long (D);
   end Set_Depth;

   procedure Set_Depth (I : in Image_Ptr; D : in Depth) is
      function C_Set_Image_Depth
        (Image : Image_Ptr;
         Depth : C.unsigned_long)
         return  C.int;
      pragma Import (C, C_Set_Image_Depth, "SetImageDepth");
      Res : C.int := 0;
   begin
      Res := C_Set_Image_Depth (I, C.unsigned_long (D));
      if Res = 0 then
         raise Depth_Error;
      end if;
   end Set_Depth;

   function Get_Depth (I : in Image_Info_Ptr) return Depth is
   begin
      return Depth (I.all.Depth);
   end Get_Depth;

   function Get_Depth (I : in Image_Ptr) return Depth is
   begin
      return Depth (I.all.Depth);
   end Get_Depth;

   procedure Set_Image_Size (I : in Image_Info_Ptr; Im_S : in Image_Size) is
      Str_X   : String       := Image_Size_T'Image (Im_S.X);
      Str_Y   : String       := Image_Size_T'Image (Im_S.Y);
      Str_X_Y : String       :=
         (Str_X (Str_X'First + 1 .. Str_X'Last) &
          'x' &
          Str_Y (Str_Y'First + 1 .. Str_Y'Last));
      X_Y     : C.char_array := To_C (Str_X_Y);
   begin
      I.all.Size := Interfaces.C.Strings.New_Char_Array (X_Y);
   end Set_Image_Size;

   function Get_Image_Size (I : in Image_Ptr) return Image_Size is
      Ims : Image_Size;
   begin
      Ims.X := Image_Size_T (I.all.Columns);
      Ims.Y := Image_Size_T (I.all.Rows);
      return Ims;
   end Get_Image_Size;

end G2F.IO;
