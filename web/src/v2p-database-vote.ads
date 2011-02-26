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

--  All vote oriented API (rating, CdC)

package V2P.Database.Vote is

   procedure Update_Rating
     (Uid      : in String;
      Tid      : in Id;
      Criteria : in String;
      Value    : in String);
   --  Update post rating

   function Get_Global_Rating (Tid : in Id) return Templates.Translate_Set;
   --  Get the post global rating

   function Get_User_Rating_On_Post
     (Uid : in String; Tid : in Id) return Templates.Translate_Set;
   --  Get the user rating on a specific post

   function Get_Photo_Of_The_Week return Templates.Translate_Set;
   --  Returns photo of the week

   function Get_CdC (From : in Positive) return Templates.Translate_Set;
   --  Returns all CdC photos

   function Get_CdC_Data (Tid : in Id) return Templates.Translate_Set;
   --  Returns all data about a specific CdC (score, voting users)

   function Get_CdC_Info return Templates.Translate_Set;
   --  Returns the current CdC information

   procedure Toggle_Vote_Week_Photo (Uid : in String; Tid : in Id);
   --  Set or Reset user Uid vote for photo Tid

   function Has_User_Vote (Uid : in String; Tid : in Id) return Boolean;
   --  Returns True if user Uid has voted for the given photo

   function Get_User_Voted_Photos
     (Uid : in String) return Templates.Translate_Set;
   --  Returns the translate table with the list of all voted photos for the
   --  given user.

end V2P.Database.Vote;
