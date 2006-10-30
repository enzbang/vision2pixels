------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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

with AWS.Templates;

package V2P.Database is

   use AWS;

   function Get_Threads return Templates.Translate_Set;
   --  Returns all threads for a given forum
   --  ??? we will need to select a forum

   function Get_Entry (Id : in String) return Templates.Translate_Set;
   --  Returns the full content of the entry Id

   function Get_User (Id : in String) return Templates.Translate_Set;
   --  Returns user's Id information

   function Get_Password (User : in String) return String;
   --  Returns the password for the given user. Returns the empty string if
   --  User cannot be found into the database.

end V2P.Database;
