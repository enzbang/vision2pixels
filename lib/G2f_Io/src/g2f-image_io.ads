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

with G2f;

package G2f.Image_Io is

   Clone_ImageInfo_Error,
   Read_Image_Error,
   Write_Image_Error,
   Ping_Image_Error:exception;

   -- image_info

   function Clone_Image_Info (Image_Info_In: in Image_Info_Ptr) return Image_Info_Ptr;

   procedure Destroy_Image_Info (Image_Info_In: in out Image_Info_Ptr);

   -- /image_info

   -- image

   function Read_Image (I:in Image_Info_Ptr) return Image_Ptr;

   procedure Write_Image(I:in Image_Info_Ptr;
                         E:in Image_Ptr);

   procedure Destroy_Image (Image_In:in out Image_Ptr);

   function Ping_Image (I:in Image_Info_Ptr) return Image_Ptr;

   -- /image

end G2f.Image_Io;
