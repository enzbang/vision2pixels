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

with Ada.Strings.Fixed;

with V2P.Settings;
with V2P.Template_Defs.Set_Global;
with V2P.Navigation_Links;

package body V2P.Context is

   use V2P;

   --------------------------
   -- Set_User_Preferences --
   --------------------------

   procedure Set_User_Preferences
     (Context   : not null access Object;
      SID       : in Session.Id;
      User_Data : in Database.User_Data) is
   begin
      Context.Set_Value
        (Template_Defs.Set_Global.FILTER,
         Database.Filter_Mode'Image (User_Data.Preferences.Filter));

      V2P.Navigation_Links.Context_Page_Size.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE,
         Value   => User_Data.Preferences.Page_Size);

      Context.Set_Value
        (Template_Defs.Set_Global.FORUM_SORT,
         Database.Forum_Sort'Image (User_Data.Preferences.Sort));

      Session.Set (SID, Template_Defs.Set_Global.ADMIN, User_Data.Admin);
   end Set_User_Preferences;

   ------------
   -- Update --
   ------------

   procedure Update
     (Context : not null access Object;
      SID     : in Session.Id;
      Cookie  : in String)
   is
      function Cookie_Content (S : in String) return String;
      --  Returns the part between "v2p=" and ";" or the empty string if the
      --  v2p cookie is not found.

      --------------------
      -- Cookie_Content --
      --------------------

      function Cookie_Content (S : in String) return String is
         use Ada.Strings.Fixed;

         Content_Start : constant Natural := Index (S, "v2p=");
         Content_End   : Natural;
      begin
         if Content_Start = 0 then
            return "";

         else
            Content_End := Index (S (Content_Start .. S'Last), ";");
            if Content_End = 0 then
               --  No ; after v2p=
               return S (Content_Start + 4 .. S'Last);
            end if;
            return S (Content_Start + 4 .. Content_End - 1);
         end if;
      end Cookie_Content;

   begin
      --  Read cookie

      if not Session.Exist (SID, Template_Defs.Set_Global.LOGIN)
        or else not Context.Exist ("cookie")
      then
         Read_Cookie : declare
            Cookie_User : constant String :=
                            Database.Get_User_From_Cookie
                              (Cookie_Content (Cookie));
         begin
            if Cookie_User /= "" then
               --  No session login but a cookie is sent, recover the session
               --  from the database.

               Session.Set (SID, Template_Defs.Set_Global.LOGIN, Cookie_User);

               --  Set last logged status

               Database.Set_Last_Logged (Cookie_User);

               --  Recover user's preferences

               Set_User_Preferences
                 (Context, SID, Database.Get_User_Data (Cookie_User));

               --  Record that the cookie is set as we do not want to generate
               --  a new one.

               Context.Set_Value ("cookie", "set");
            end if;
         end Read_Cookie;
      end if;

      if not Context.Exist (Template_Defs.Set_Global.FILTER)
        or else not Context.Exist (Template_Defs.Set_Global.FILTER_PAGE_SIZE)
        or else not Context.Exist (Template_Defs.Set_Global.FORUM_SORT)
      then
         if Session.Exist (SID, Template_Defs.Set_Global.LOGIN) then
            --  Session is set but there is no preferences set yet, set user's
            --  preferences.

            Set_User_Preferences
              (Context,
               SID,
               Database.Get_User_Data
                 (Session.Get (SID, Template_Defs.Set_Global.LOGIN)));

            Context.Set_Value
              (Template_Defs.Set_Global.FILTER_CATEGORY, "");

         else
            Set_User_Preferences (Context, SID, Database.No_User_Data);
         end if;
      end if;

      --  Set timezone

      if not Context.Exist (Template_Defs.Set_Global.TZ) then
         declare
            TZ : constant String :=
                   Session.Get (SID, Template_Defs.Set_Global.TZ);
         begin
            if TZ = "" then
               Context.Set_Value (Template_Defs.Set_Global.TZ, "+0");
            else
               Context.Set_Value (Template_Defs.Set_Global.TZ, TZ);
            end if;
         end;
      end if;

      if not Context.Exist (Template_Defs.Set_Global.NAV_FROM)
        or else not Context.Exist (Template_Defs.Set_Global.ORDER_DIR)
      then
         Not_Null_Counter.Set_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.NAV_FROM,
            Value   => 1);

         --  Should be in config

         if Settings.Descending_Order then
            Context.Set_Value
              (Template_Defs.Set_Global.ORDER_DIR,
               Database.Order_Direction'Image (Database.DESC));
         else
            Context.Set_Value
              (Template_Defs.Set_Global.ORDER_DIR,
               Database.Order_Direction'Image (Database.ASC));
         end if;
      end if;

      --  Set LOGIN and ADMIN in session

      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN) then
         Context.Set_Value
           (Template_Defs.Set_Global.LOGIN,
            Session.Get (SID, Template_Defs.Set_Global.LOGIN));
      else
         Context.Remove (Template_Defs.Set_Global.LOGIN);
      end if;

      if Session.Exist (SID, Template_Defs.Set_Global.ADMIN) then
         Context.Set_Value
           (Template_Defs.Set_Global.ADMIN,
            Boolean'Image
              (Boolean'(Session.Get (SID, Template_Defs.Set_Global.ADMIN))));
      else
         Context.Remove (Template_Defs.Set_Global.ADMIN);
      end if;
   end Update;

end V2P.Context;
