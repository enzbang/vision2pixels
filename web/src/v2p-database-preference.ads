------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2010-2011                          --
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

package V2P.Database.Preference is

   procedure User
     (Login       : in     String;
      Preferences :    out User_Settings);
   --  Returns the user's preferences for the given user. If no preferences are
   --  set, use the default values.

   procedure Set_CSS_URL (Login : in String; URL : in String);
   --  Set css url for the given user

   procedure Set_Avatar
     (Login  : in String;
      Avatar : in String);
   --  Set avatar preference for the given user

   procedure Set_Comment_Visible
     (Login                 : in String;
      Start_Comment_Visible : in Boolean);

   procedure Set_Filter
     (Login  : in String;
      Filter : in Filter_Mode);
   --  Set filter preference for the given user

   procedure Set_Filter_Page_Size
     (Login     : in String;
      Page_Size : in Positive);
   --  Set filter preference for the given user

   procedure Set_Filter_Sort
     (Login : in String;
      Sort  : in Forum_Sort);
   --  Set sort preference for the given user

   procedure Set_Image_Size
     (Login      : in String;
      Image_Size : in Database.Image_Size);
   --  Set image size preference for the given user

   procedure Set_Private_Message
     (Login                  : in String;
      Accept_Private_Message : in Boolean);
   --  Set private message preference

end V2P.Database.Preference;
