------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with AWS.Services.Web_Block.Context;
with AWS.Session;

with V2P.Database;

package V2P.Context is

   use Ada;
   use AWS;
   use AWS.Services.Web_Block.Context;

   package Counter is
     new Generic_Data (Data => Natural, Null_Data => 0);
   --  Adds natural to context value data

   package Not_Null_Counter is
     new Generic_Data (Data => Positive, Null_Data => 1);
   --  Adds positive to context value data

   procedure Update
     (Context : not null access Object;
      SID     : in AWS.Session.Id;
      Cookie  : in String);
   --  Update the context filter
   --  Set LOGIN in Context.
   --  Read Cookie ("remember me" authentication type)

   procedure Set_User_Preferences
     (Context   : not null access Object;
      SID       : in Session.Id;
      User_Data : in Database.User_Data);
   --  Set user preferences into the context after user is logged in

end V2P.Context;
