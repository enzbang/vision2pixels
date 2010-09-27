------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2009-2010                          --
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

package V2P.Database.Registration is

   function Register_User (Login, Password, Email : in String) return Boolean;
   --  Register a new user before validation. Returns False if the user cannot
   --  be registered (duplicate login).

   function Validate_User (Login, Key : in String) return Boolean;
   --  Validate a registered user

   function Delete_User (Login : in String) return Boolean;
   --  Delete user from user_to_validate table (for admin)

   function Delete_User (Login, Key : in String) return Boolean;
   --  Delete user from user_to_validate table

   function Send_Reminder
     (Login : in String := "") return Templates.Translate_Set;
   --  Send a reminder to all user to validate or just to the given user

   function Users_To_Validate return Templates.Translate_Set;
   --  Returns all users in the users_to_validate table

   function Get_User_To_Validate_Data (Uid : in String) return User_Data;
   --  As above but for user not yet validated

   procedure Register_New_User_Email
     (Uid : in String; New_Email : in String);
   --  Update user email

   function Validate_New_User_Email (Uid, Key : in String) return Boolean;
   --  Validate new e-mail for given user

end V2P.Database.Registration;
