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

package body V2P.Context is

   use Post_Ids;

   ----------
   -- Next --
   ----------

   function Next
     (Posts : in Post_Ids.Vector;
      Id    : in String) return String
   is
      Current : Cursor := Find (Posts, Id);
   begin
      if Current = No_Element then
         return "";
      end if;

      Next (Current);

      if Current /= No_Element then
         return Element (Current);
      else
         return "";
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Posts : in Post_Ids.Vector;
      Id    : in String) return String
   is
      Current : Cursor := Find (Posts, Id);
   begin
      if Current = No_Element then
         return "";
      end if;

      Previous (Current);

      if Current /= No_Element then
         return Element (Current);
      else
         return "";
      end if;
   end Previous;

end V2P.Context;
