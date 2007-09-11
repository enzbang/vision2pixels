------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2007                             --
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

with Ada.Containers.Indefinite_Vectors;
with AWS.Services.Web_Block.Context;

package V2P.Context is

   use Ada;
   use AWS.Services.Web_Block.Context;

   package Post_Ids is
     new Containers.Indefinite_Vectors
       (Index_Type => Positive, Element_Type => String);
   --  Post_Ids stores all visible post ids in forum threads page

   package Navigation_Links is
     new Generic_Data (Data      => Post_Ids.Vector,
                       Null_Data => Post_Ids.Empty_Vector);
   --  Adds Post_Ids.Vector to context value data

   package Navigation_From is
     new Generic_Data (Data => Integer, Null_Data => 1);
   --  Adds positive to context value data
   --  ??? Integer used here instead of Positive to work around a GNAT GPL 2006
   --  bug, should be changed when GNAT GPL 2007 is out and if it contains the
   --  fix as this bug is not present on recent GNAT versions.

   function Previous
     (Posts : in Post_Ids.Vector;
      Id    : in String) return String;
   --  Returns previous post stored in Post_Ids.Vector

   function Next
     (Posts : in Post_Ids.Vector;
      Id    : in String) return String;
   --  Returns next post stored in Post_Ids.Vector

end V2P.Context;
