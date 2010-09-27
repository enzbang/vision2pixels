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

package V2P.Database.Remember is

   function Gen_Cookie (Login : in String) return String;
   --  Generate a new random string for "remember me" authentication

   procedure Register_Cookie (Login : in String; Cookie : in String);
   --  Register a new cookie in database

   function Get_User_From_Cookie (Cookie : in String) return String;
   --  Return user associated with the given cookie (or "")

   procedure Delete_User_Cookies (Login : in String);
   --  Delete user cookies

   procedure Set (Login : in String; Status : in Boolean);
   --  Should we remember the user with a cookie ?

end V2P.Database.Remember;
