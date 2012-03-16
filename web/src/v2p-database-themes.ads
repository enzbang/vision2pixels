------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2012                            --
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

package V2P.Database.Themes is

   type Stage is (Open, Stage_1, Stage_2, Closed);
   --     Open    : A theme is created, vote not yet activated
   --     Stage_1 : Vote for stage 1 activated
   --     Stage_2 : Vote for stage 2 activated
   --     Closed  : No current theme running

   subtype Theme_Running is Stage range Open .. Stage_2;

   function Current_Stage return Stage;
   --  Returns current stage for the theme

   function Create (Title : in String) return Boolean;
   --  Create a new theme, this can only be done if there is no current theme
   --  opened. Returns False if it was not possible to create a theme (probably
   --  because another user has created one in the meam-time).

   function Get_Current_Status (TZ : in String) return Templates.Translate_Set;
   --  Returns data for the current theme, when calling this routine one theme
   --  must be running.

   function Get_Admin_Status return Templates.Translate_Set;
   --  Returns some data for the current stage for admin, these data should
   --  help the admin to see if it is possible to move to next stage.

   function Get_Current_Photos
     (Login : in String) return Templates.Translate_Set;
   --  Returns the photos for the current theme, returns also the vote for each
   --  photos for the given user if given.

   function Get_Themes return Templates.Translate_Set;
   --  Returns the list of closed themes

   function Get_Theme_Data
     (TID : in Id; TZ : in String) return Templates.Translate_Set;
   --  Returns all data for a specific theme

   procedure Set_Reset_Vote (Login, Photo_Id : in String);
   --  Set or reset a vote for the them for the given user

   function Get_Vote_Count (Login : in String) return Natural;
   --  Returns the number of vote for the current user (for the current theme)

   procedure Next_Stage;
   --  Move current theme to next stage, does nothing if this is no stage open

end V2P.Database.Themes;
