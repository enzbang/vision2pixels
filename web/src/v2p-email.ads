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

package V2P.Email is

   Cannot_Send : exception;

   procedure Send_Register_User (Login, Password, Email : in String);
   --  Send message for registration

   procedure Send_Reminder (Login, Password, Email : in String);
   --  Send reminder message for the registration (users not yet validated)

   procedure Send_Lost_Password (Login, Password, Email : in String);
   --  Send password to given address

   procedure Send_Private_Message (From, Login, Email, Message : in String);
   --  Send private message from From to (Login, Email)

   procedure Send_Change_Email (Login, Email, New_Email : in String);
   --  Send message for new email validation

   function Reminder_Message
     (Login, Password, Email : in String) return String;
   --  Build and return the reminder message

end V2P.Email;
