------------------------------------------------------------------------------
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

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Interfaces.C.Strings;                 use Interfaces.C.Strings;

package body G2F is

   procedure Initialize_Magick is
      procedure InitializeMagick
        (Path : in Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.Null_Ptr);
      pragma Import (C, InitializeMagick, "InitializeMagick");
   begin
      InitializeMagick;
   end Initialize_Magick;

   function Get_Exception_Info
     (G_E_I : in Exception_Info)
      return  Exception_Info_Ptr
   is
      procedure C_Get_Exception_Info (E : in System.Address);
      pragma Import (C, C_Get_Exception_Info, "GetExceptionInfo");
      Ex_Info_Ptr : Exception_Info_Ptr := null;
      package Info_Ptrs is new System.Address_To_Access_Conversions (
         Exception_Info);
      use Info_Ptrs;
   begin
      C_Get_Exception_Info (G_E_I'Address);
      if Info_Ptrs.To_Pointer (G_E_I'Address) = null then
         raise GetExceptionInfo_Error;
      end if;
      Ex_Info_Ptr := new Exception_Info'(G_E_I);
      return Ex_Info_Ptr;
   end Get_Exception_Info;

   procedure Destroy_Magick is
      procedure C_Destroy_Magick;
      pragma Import (C, C_Destroy_Magick, "DestroyMagick");
      procedure Ada_Free is new Ada.Unchecked_Deallocation (
         Exception_Info,
         Exception_Info_Ptr);
   begin
      C_Destroy_Magick;
      Ada_Free (Ex_Info_Ptr);
   end Destroy_Magick;

   procedure Free (I : in out Image_Info_Ptr) is
      procedure Ada_Free is new Ada.Unchecked_Deallocation (
         Image_Info,
         Image_Info_Ptr);
   begin
      Ada_Free (I);
   end Free;

   procedure Free (I : in out Image_Ptr) is
      procedure Ada_Free is new Ada.Unchecked_Deallocation (
         Image,
         Image_Ptr);
   begin
      Ada_Free (I);
   end Free;

   function Is_Null (I : in Image_Ptr) return Boolean is
   begin
      return I = null;
   end Is_Null;

   function Is_Null (I : in Image_Info_Ptr) return Boolean is
   begin
      return I = null;
   end Is_Null;

   procedure Put_Magick_Exception is
      procedure C_Catch_Exception (E : in Exception_Info_Ptr);
      pragma Import (C, C_Catch_Exception, "CatchException");
   begin
      C_Catch_Exception (Ex_Info_Ptr);
      -- Ada.Text_Io.New_Line;
      -- Ada.Text_Io.Put_Line("Magick_Exception : ");
      -- Ada.Text_Io.Put("Reason => " &
      --Interfaces.C.Strings.Value(Ex_Info_Ptr.all.Reason));
      -- Ada.Text_Io.Put_Line(" " &
      --Interfaces.C.Strings.Value(Ex_Info_Ptr.all.Description));
      -- Ada.Text_Io.Put_Line("Error Number =>" &
      --C.Int'Image(Ex_Info_Ptr.all.Error_Number));
      ---- Ada.Text_Io.Put_Line("Severity => " &
      --Exception_Type'Image(Ex_Info_Ptr.all.Severity));
      -- Ada.Text_Io.Put_Line("Severity => " &
      --C.Int'Image(Ex_Info_Ptr.all.Severity));
   end Put_Magick_Exception;

   procedure Put_Image_Exception (I : in Image_Ptr) is
   begin
      if I.all.Image_Exception.Reason /= Interfaces.C.Strings.Null_Ptr then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Image_Exception : ");
         Ada.Text_IO.Put
           ("Reason => " &
            Interfaces.C.Strings.Value (I.all.Image_Exception.Reason));
         Ada.Text_IO.Put_Line
           (" " &
            Interfaces.C.Strings.Value (I.all.Image_Exception.Description));
         Ada.Text_IO.Put_Line
           ("Error Number =>" &
            C.int'Image (I.all.Image_Exception.Error_Number));
         Ada.Text_IO.Put_Line
           ("Severity => " & C.int'Image (I.all.Image_Exception.Severity));
         --Ada.Text_Io.Put_Line("Severity => " &
         --Exception_Type'Image(Ex_Info_Ptr.all.Severity));
      end if;
   end Put_Image_Exception;
begin
   Initialize_Magick;
   Ex_Info_Ptr := Get_Exception_Info (Ex_Info);
end G2F;
