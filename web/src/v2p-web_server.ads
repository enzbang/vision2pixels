------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
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

package V2P.Web_Server is

   Images_Source_Prefix : constant String := "/photos";
   --  Source prefix used to reference images in URL

   Thumbs_Source_Prefix : constant String := "/thumbs";
   --  Source prefix used to reference thumbs in URL

   procedure Start;
   --  Start the Web Server, port is taken from the ini file

   procedure Wait;
   --  Wait forever, the server needs to be killed

   procedure Stop;
   --  Stop the server and returns

end V2P.Web_Server;
