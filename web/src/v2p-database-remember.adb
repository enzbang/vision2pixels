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

with Ada.Characters.Handling;

with AWS.Utils;

with V2P.DB_Handle;
with V2P.Database.Support;

package body V2P.Database.Remember is

   use Ada;

   use V2P.Database.Support;

   -------------------------
   -- Delete_User_Cookies --
   -------------------------

   procedure Delete_User_Cookies (Login : in String) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute
        ("DELETE FROM remember_user WHERE user_login=" & Q (Login));
   end Delete_User_Cookies;

   ----------------
   -- Gen_Cookie --
   ----------------

   function Gen_Cookie (Login : in String) return String is
      Cookie_Value : constant String :=
                       Characters.Handling.To_Lower (Utils.Random_String (16));
   begin
      Register_Cookie (Login, Cookie_Value);
      return Cookie_Value;
   end Gen_Cookie;

   --------------------------
   -- Get_User_From_Cookie --
   --------------------------

   function Get_User_From_Cookie (Cookie : in String) return String is
      DBH  : constant TLS_DBH_Access :=
               TLS_DBH_Access (DBH_TLS.Reference);
      SQL  : constant String :=
               "SELECT user_login "
                 & "FROM remember_user, user "
                 & "WHERE cookie_content=" & Q (Cookie)
                 & " AND remember='TRUE' AND user.login=user_login";
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      if not Iter.More then
         return "";
      end if;

      Iter.Get_Line (Line);

      declare
         Login : constant String := DB.String_Vectors.Element (Line, 1);
      begin
         Line.Clear;

         --  Now update the timestamp for this cookie
         DBH.Handle.Execute
           ("UPDATE remember_user SET last_used=current_timestamp"
            & " WHERE user_login=" & Q (Login)
            & " AND cookie_content=" & Q (Cookie));
         return Login;
      end;
   end Get_User_From_Cookie;

   ---------------------
   -- Register_Cookie --
   ---------------------

   procedure Register_Cookie (Login : in String; Cookie : in String) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      DBH.Handle.Execute
        ("INSERT INTO remember_user ('user_login', 'cookie_content') VALUES ("
         & Q (Login) & ", " & Q (Cookie) & ")");
   end Register_Cookie;

   ---------
   -- Set --
   ---------

   procedure Set (Login : in String; Status : in Boolean) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "UPDATE user SET remember=" & Q (Status)
              & " WHERE login=" & Q (Login);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Set;

end V2P.Database.Remember;
