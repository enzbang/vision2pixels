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

with AWS.Utils;

with V2P.Database.Support;
with V2P.DB_Handle;

package body V2P.Database.Preference is

   use V2P.Database.Support;

   function Preferences_Exist (Uid : in String) return Boolean;
   --  Returns True if a current set of user's preferences exist

   procedure Set
     (Login       : in String;
      Name, Value : in String);
   --  Update the user preferences named Name with the given Value. If not
   --  preferences are registered for the given user a new set of preferences
   --  are inserted. This code is used by all procedure which need to set a
   --  preferences.

   -----------------------
   -- Preferences_Exist --
   -----------------------

   function Preferences_Exist (Uid : in String) return Boolean is
      SQL    : constant String :=
                 "SELECT 1 FROM user_preferences WHERE user_login=" & Q (Uid);
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Result : Boolean;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      Result := Iter.More;
      Iter.End_Select;
      return Result;
   end Preferences_Exist;

   ---------
   -- Set --
   ---------

   procedure Set
     (Login       : in String;
      Name, Value : in String)
   is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
   begin
      Connect (DBH);

      if Preferences_Exist (Login) then
         DBH.Handle.Execute
           ("UPDATE user_preferences SET " & Name & '=' & Value
            & " WHERE user_login=" & Q (Login));
      else
         DBH.Handle.Execute
           ("INSERT INTO user_preferences ('user_login', '" & Name & "') "
            & "VALUES (" & Q (Login) & ", " & Value & ')');
      end if;
   end Set;

   ----------------
   -- Set_Avatar --
   ----------------

   procedure Set_Avatar
     (Login  : in String;
      Avatar : in String) is
   begin
      Set (Login, "avatar", Q (Avatar));
   end Set_Avatar;

   -----------------
   -- Set_CSS_URL --
   -----------------

   procedure Set_CSS_URL
     (Login : in String; URL : in String) is
   begin
      Set (Login, "css_url", Q (URL));
   end Set_CSS_URL;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Login  : in String;
      Filter : in Filter_Mode) is
   begin
      Set (Login, "filter", Q (Filter_Mode'Image (Filter)));
   end Set_Filter;

   --------------------------
   -- Set_Filter_Page_Size --
   --------------------------

   procedure Set_Filter_Page_Size
     (Login     : in String;
      Page_Size : in Positive) is
   begin
      Set (Login, "photo_per_page", Utils.Image (Page_Size));
   end Set_Filter_Page_Size;

   ---------------------
   -- Set_Filter_Sort --
   ---------------------

   procedure Set_Filter_Sort
     (Login : in String;
      Sort  : in Forum_Sort) is
   begin
      Set (Login, "sort", Q (Forum_Sort'Image (Sort)));
   end Set_Filter_Sort;

   --------------------
   -- Set_Image_Size --
   --------------------

   procedure Set_Image_Size
     (Login      : in String;
      Image_Size : in Database.Image_Size) is
   begin
      Set (Login, "image_size", Q (Database.Image_Size'Image (Image_Size)));
   end Set_Image_Size;

   -------------------------
   -- Set_Private_Message --
   -------------------------

   procedure Set_Private_Message
     (Login                  : in String;
      Accept_Private_Message : in Boolean) is
   begin
      Set (Login, "accept_private_message",
           Q (Boolean'Image (Accept_Private_Message)));
   end Set_Private_Message;

   ----------
   -- User --
   ----------

   procedure User
     (Login       : in     String;
      Preferences :    out User_Settings)
   is
      SQL  : constant String :=
               "SELECT photo_per_page, filter, sort, image_size, css_url, "
                 & "accept_private_message, avatar "
                 & "FROM user_preferences WHERE user_login=" & Q (Login);
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);

         Preferences :=
           User_Settings'
             (Page_Size => Positive'Value
                  (DB.String_Vectors.Element (Line, 1)),
              Filter    => Filter_Mode'Value
                (DB.String_Vectors.Element (Line, 2)),
              Sort      => Forum_Sort'Value
                (DB.String_Vectors.Element (Line, 3)),
              Image_Size => Image_Size'Value
                (DB.String_Vectors.Element (Line, 4)),
              CSS_URL    => To_Unbounded_String
                (DB.String_Vectors.Element (Line, 5)),
              Accept_Private_Message => Boolean'Value
                ((DB.String_Vectors.Element (Line, 6))),
              Avatar                 => To_Unbounded_String
                (DB.String_Vectors.Element (Line, 7)));

      else
         Preferences := Default_User_Settings;
      end if;

      Iter.End_Select;
   end User;

end V2P.Database.Preference;
