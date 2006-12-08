------------------------------------------------------------------------------
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

with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;

package body G2F.Image_Text_Attribute is

   use Ada;

   ------------------------------
   -- Set_Image_Text_Attribute --
   ------------------------------

   procedure Set_Image_Text_Attribute
     (I     : in out Image_Ptr;
      Key   : in Tag_Attribute;
      Value : in String)
   is
      use C;
      use Interfaces.C.Strings;

      function C_Set_Image_Text_Attribute
        (I          : in Image_Ptr;
         Key, Value : in Interfaces.C.Strings.chars_ptr) return C.int;
      pragma Import (C, C_Set_Image_Text_Attribute, "SetImageAttribute");

      Res   : C.int := 0;
      Key_S : String :=
                Characters.Handling.To_Lower (Tag_Attribute'Image (Key));
   begin
      Key_S (1) := Ada.Characters.Handling.To_Upper (Key_S (1));
      Res       :=
         C_Set_Image_Text_Attribute
           (I,
            New_Char_Array (To_C (Key_S)),
            New_Char_Array (To_C (Value)));
      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Attribute_Error;
      end if;
   end Set_Image_Text_Attribute;

   ------------------------------
   -- Set_Image_Text_Attribute --
   ------------------------------

   procedure Set_Image_Text_Attribute
     (I          : in out Image_Ptr;
      Key, Value : in String)
   is
      use C;
      use Interfaces.C.Strings;

      function C_Set_Image_Text_Attribute
        (I          : in Image_Ptr;
         Key, Value : in Interfaces.C.Strings.chars_ptr) return C.int;

      Res : C.int := 0;
      pragma Import (C, C_Set_Image_Text_Attribute, "SetImageAttribute");
   begin
      Res := C_Set_Image_Text_Attribute
        (I, New_Char_Array (To_C (Key)), New_Char_Array (To_C (Value)));

      if Res = 0 or else I.all.Image_Exception.Severity /= 0 then
         raise Attribute_Error;
      end if;
   end Set_Image_Text_Attribute;

   ------------------------------
   -- Get_Image_Text_Attribute --
   ------------------------------

   function Get_Image_Text_Attribute
     (I    : in Image_Ptr;
      Key  : in Tag_Attribute) return String
   is
      use C;
      use Interfaces.C.Strings;

      function C_Get_Image_Attribute
        (I   : in Image_Ptr;
         Key : in Interfaces.C.Strings.chars_ptr) return Image_Attribute_Ptr;
      pragma Import (C, C_Get_Image_Attribute, "GetImageAttribute");

      Res : Image_Attribute_Ptr := null;

   begin
      Res :=
         C_Get_Image_Attribute
           (I,
            New_Char_Array (To_C (Tag_Attribute'Image (Key))));
      if Res = null then
         return "";
      end if;
      return To_Ada (Value (Res.all.Value));
   end Get_Image_Text_Attribute;

   ------------------------------
   -- Get_Image_Text_Attribute --
   ------------------------------

   function Get_Image_Text_Attribute
     (I   : in Image_Ptr;
      Key : in String) return String
   is
      use C;
      use Interfaces.C.Strings;

      function C_Get_Image_Attribute
        (I   : in Image_Ptr;
         Key : in Interfaces.C.Strings.chars_ptr) return Image_Attribute_Ptr;
      pragma Import (C, C_Get_Image_Attribute, "GetImageAttribute");

      Res : Image_Attribute_Ptr := null;

   begin
      Res := C_Get_Image_Attribute (I, New_Char_Array (To_C (Key)));
      if Res = null then
         return "";
      end if;
      return To_Ada (Value (Res.all.Value));
   end Get_Image_Text_Attribute;

end G2F.Image_Text_Attribute;
