------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

package V2P.URL is

   function User_Name (URL : in String) return String;
   --  Returns User_Name from URL

   function Big_Images_Full_Prefix return String;
   --  Returns images full prefix : Vision2Pixels plugin path + image path
   --  or image path if it is an absolute path.
   --  Removes the trailing '/' if it exists.

   function Medium_Images_Full_Prefix return String;
   --  Returns images full prefix : Vision2Pixels plugin path + image path
   --  or image path if it is an absolute path.
   --  Removes the trailing '/' if it exists.

   function Thumbs_Full_Prefix return String;
   --  Returns thumbs full prefix : Vision2Pixels plugin path + thumbs path
   --  or thumbs path if it is an absolute path
   --  Removes the trailing '/' if it exists.

   function Avatar_Full_Prefix return String;
   --  Returns avatar full prefix : Vision2Pixels plugin path + avatar path
   --  or avatar path if it is an absolute path
   --  Removes the trailing '/' if it exists.

end V2P.URL;
