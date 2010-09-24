------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2010                            --
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

with Morzhol.OS;

package body V2P.Utils is

   -------------------
   -- Clean_Mapping --
   -------------------

   function Clean_Mapping (From : in Character) return Character is
   begin
      if From in 'a' .. 'z'
        or else From in 'A' .. 'Z'
        or else From in '0' .. '9'
        or else Morzhol.OS.Is_Directory_Separator (From)
        or else From = '.'
      then
         return From;
      else
         return 'x';
      end if;
   end Clean_Mapping;

end V2P.Utils;
