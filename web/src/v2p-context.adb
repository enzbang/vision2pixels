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

with V2P.Database;
with V2P.Settings;
with V2P.Template_Defs.Set_Global;

package body V2P.Context is

   use V2P;
   use AWS;

   ------------
   -- Update --
   ------------

   procedure Update
     (Context : not null access Object;
      SID     : in Session.Id;
      Cookie  : in String)
   is
      function Cookie_Content (S : in String) return String;
      --  Returns the part between v2p= and ;

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

      if not Session.Exist (SID, Template_Defs.Set_Global.LOGIN) then
         Read_Cookie : declare
            Cookie_User : constant String :=
                            Database.Get_User_From_Cookie
                              (Cookie_Content (Cookie));
         begin
            if Cookie_User /= "" then
               Session.Set (SID, Template_Defs.Set_Global.LOGIN, Cookie_User);
               Context.Set_Value ("cookie", "set");
            end if;
         end Read_Cookie;
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

      --  Set default filter

      if not Context.Exist (Template_Defs.Set_Global.FILTER) then
         Context.Set_Value
           (Template_Defs.Set_Global.FILTER,
            Database.Filter_Mode'Image (Database.Seven_Days));

         Context.Set_Value
           (Template_Defs.Set_Global.FILTER_CATEGORY, "");

         Context.Set_Value
           (Template_Defs.Set_Global.FORUM_SORT,
            Database.Forum_Sort'Image (Database.Last_Posted));

         Not_Null_Counter.Set_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE,
            Value   => 10);

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
   end Update;

end V2P.Context;
