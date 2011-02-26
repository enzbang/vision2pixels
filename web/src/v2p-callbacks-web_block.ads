------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2011                          --
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

with AWS.Status;
with AWS.Templates;
with AWS.Services.Web_Block.Context;

package V2P.Callbacks.Web_Block is

   use AWS;

   procedure CdC
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure CdC_Data
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure CdC_Info
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure CdC_Alert
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) renames CdC_Info;

   procedure Exif
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Category_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Category_Set
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Filter_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Photo_List_Select
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Text_List_Select
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Threads
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Forum_Threads_Text
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
      renames Forum_Threads;

   procedure Latest_Posts
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Latest_Users
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Global_Rating
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Login
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure Metadata
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_New_Avatar
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure Pref_Filters
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure New_Comment
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure New_Vote
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure New_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure New_Post
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Photo_Of_The_Week
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Quick_Login
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_User_Email
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Avatar
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Comment_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Message_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Photo_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Preferences
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure User_Stats
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure User_Voted_Photos_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Users_To_Validate
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Vote_Week_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_CSS_URL
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_Forum_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_Forum_Filter_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_Image_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Users
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Private_Message
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Pref_Private_Message
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
      renames Private_Message;

end V2P.Callbacks.Web_Block;
