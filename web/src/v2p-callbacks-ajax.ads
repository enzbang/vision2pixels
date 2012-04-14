------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2012                          --
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

package V2P.Callbacks.Ajax is

   use AWS;

   procedure Login
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Login callback

   procedure Logout
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Logout callback

   procedure Onchange_Forum_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when a new forum is selected on post page

   procedure Onchange_Filter_Forum
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum filter

   procedure Onchange_Filter_Forum_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum filter preference in user's page

   procedure Onchange_Filter_Forum_Page_Size_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum filter page size preference in user's
   --  page.

   procedure Onchange_Filter_Sort_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum sort preference in user's page

   procedure Onchange_Filter_Forum_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum page size limit

   procedure Onchange_Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum sorting

   procedure Onchange_Image_Size_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the image size preference in user's page

   procedure Onchange_Category_Filter_Forum
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum category filter

   procedure Onchange_Category_Set
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the category of a post

   procedure Onclick_Pref_Private_Message_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing private message preference

   procedure Onclick_Pref_Show_Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing show comments preference

   procedure Onclick_Send_Reminders
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on button to send all reminders

   procedure Onclick_Show_Reminder
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on button to show a reminder

   procedure Onclick_Delete_User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on button to delete a not yet registered user

   procedure Onclick_CSS_URL_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the default css url

   procedure Onclick_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on the next page link

   procedure Onclick_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on the previous page link

   procedure Onclick_User_Photo_List_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on user's photo list navigation next page

   procedure Onclick_User_Photo_List_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on user's photo list navigation previous page

   procedure Onclick_Users_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on user's the next page link

   procedure Onclick_Users_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on user's the previous page link

   procedure Onclick_CdC_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on CdC's navigation next page

   procedure Onclick_CdC_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when clicking on CdC's navigation previous page

   procedure Onclick_Users_Sort_Registered_On
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when sorting on registered date

   procedure Onclick_Users_Sort_Last_Connected
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when sorting on last connected date

   procedure Onclick_Users_Sort_Nb_Photos
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when sorting on number of photos posted

   procedure Onclick_Users_Sort_Nb_Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when sorting on number of comments posted

   procedure Onclick_Users_Sort_Nb_CdC
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when sorting on number of CdCs

   procedure Onclick_Hidden_Status_Toggle
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when changing the forum sorting

   procedure Onclick_Vote_Week_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when registering/reseting a weekly vote

   procedure Onclick_Theme_Vote
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Vote for a photo on the current theme

   procedure Onclick_Theme_Next_Stage
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Move to the next stage

   procedure Onsubmit_Comment_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting a new comment

   procedure Onsubmit_Metadata_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting new metadata

   procedure Onsubmit_Post_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting a new post

   procedure Onsubmit_User_Email_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when a user change its email

   procedure Onsubmit_User_Page_Edit_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting new user page content

   procedure Onsubmit_Rate
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting a new rating

   procedure Onsubmit_Plp_Lost_Password
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when a user ask for a lost password

   procedure Onsubmit_Pur_Register_User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when a user register itself

   procedure Onsubmit_Search_Form
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Called when submitting a new rating

   procedure Onsubmit_Private_Message
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Send a private message

   procedure Onsubmit_New_Theme
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);
   --  Create a new theme

   procedure On_Context_Error
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;
   --  Context error callback

end V2P.Callbacks.Ajax;
