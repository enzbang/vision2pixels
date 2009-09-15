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

with Ada.Strings.Unbounded;

with GNAT.String_Split;

with AWS.Parameters;
with AWS.Session;

with Image.Metadata.Geographic;

with Morzhol.Logs;
with Morzhol.Strings;

with V2P.Context;
with V2P.Database.Registration;
with V2P.Database.Search;
with V2P.Email;
with V2P.Navigation_Links;
with V2P.Settings;
with V2P.Wiki;

with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_New_Text_Entry;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Page_Search;
with V2P.Template_Defs.Page_Lost_Password;
with V2P.Template_Defs.Page_User_Register;
with V2P.Template_Defs.Set_Global;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_Forum_Category_Filter;
with V2P.Template_Defs.Block_Forum_Category_Set;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_Forum_Filter_Page_Size;
with V2P.Template_Defs.Block_Forum_Sort;
with V2P.Template_Defs.Block_Pref_Css_Url;
with V2P.Template_Defs.Block_Pref_Forum_Filter;
with V2P.Template_Defs.Block_Pref_Forum_Filter_Page_Size;
with V2P.Template_Defs.Block_Pref_Forum_Sort;
with V2P.Template_Defs.Block_Pref_Image_Size;
with V2P.Template_Defs.Block_Pref_Private_Message;
with V2P.Template_Defs.Block_Private_Message;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_Users_To_Validate;
with V2P.Template_Defs.Chunk_Forum_List_Select;
with V2P.Template_Defs.Chunk_Search_User;
with V2P.Template_Defs.Chunk_Search_Comment;
with V2P.Template_Defs.Chunk_Search_Post;
with V2P.Template_Defs.Chunk_Search_Text_Post;
with V2P.Template_Defs.R_Block_Forum_Filter;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Post_Form_Enter;
with V2P.Template_Defs.R_Block_Pref_Private_Message;
with V2P.Template_Defs.R_Block_Send_Private_Message;
with V2P.Template_Defs.R_Block_Comment_Form_Enter;
with V2P.Template_Defs.R_Block_User_Page_Edit_Form_Enter;
with V2P.Template_Defs.R_Page_Search;
with V2P.Template_Defs.R_Page_Lost_Password;
with V2P.Template_Defs.R_Page_User_Register;
with V2P.Template_Defs.R_Block_User_Preferences;
with V2P.Template_Defs.Set_Values;

package body V2P.Callbacks.Ajax is

   use Ada.Strings.Unbounded;
   use GNAT;

   Module : constant Morzhol.Logs.Module_Name := "Ajax";

   procedure User_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      Sort_On      : in              Database.User_Sort);
   --  Handle user sort

   -----------
   -- Login --
   -----------

   procedure Login
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use type Database.User_Data;
      SID       : constant Session.Id := Status.Session (Request);
      P         : constant Parameters.List := Status.Parameters (Request);
      Login     : constant String :=
                    Parameters.Get
                      (P, Template_Defs.Block_Login.HTTP.bl_login_input);
      User_Data : constant Database.User_Data :=
                    Database.Get_User_Data (Login);
   begin
      if User_Data /= Database.No_User_Data
        and then
          To_String (User_Data.Password) =
          Parameters.Get (P, Template_Defs.Block_Login.HTTP.bl_password_input)
      then
         Session.Set (SID, Template_Defs.Set_Global.LOGIN, Login);
         Session.Set
           (SID,
            Template_Defs.Set_Global.PASSWORD, To_String (User_Data.Password));

         --  Set last logged status

         Database.Set_Last_Logged (Login);

         --  Maybe remember user

         if Parameters.Get
           (P, Template_Defs.Block_Login.HTTP.bl_remember_me) /= ""
         then
            Database.Remember (Login, True);
         else
            Database.Remember (Login, False);
         end if;

         --  Set user's filtering preference

         V2P.Context.Set_User_Preferences (Context, SID, User_Data);

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Login.LOGIN,
               String'(Session.Get (SID, Template_Defs.Set_Global.LOGIN))));
      end if;
   end Login;

   ------------
   -- Logout --
   ------------

   procedure Logout
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      SID    : constant Session.Id := Status.Session (Request);
      Login  : constant String :=
                 Session.Get (SID, Template_Defs.Set_Global.LOGIN);
   begin
      Session.Delete (SID);

      Context.Remove (Template_Defs.Set_Global.LOGIN);
      Context.Remove (Template_Defs.Set_Global.ADMIN);

      --  Remove the login information from the translate table

      Templates.Remove (Translations, Template_Defs.Set_Global.LOGIN);

      --  When user want to logout he certainly does not want to be remembered
      Database.Remember (Login, False);
      Database.Delete_User_Cookies (Login);
   end Logout;

   ------------------------------------
   -- Onchange_Category_Filter_Forum --
   ------------------------------------

   procedure Onchange_Category_Filter_Forum
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Category_Filter.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bfcf_forum_category_filter_set);
   begin
      --  Keep the sorting scheme into the session

      Context.Set_Value (Template_Defs.Set_Global.FILTER_CATEGORY, Filter);

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));

      --  Reset FROM

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => 1);
   end Onchange_Category_Filter_Forum;

   ---------------------------
   -- Onchange_Category_Set --
   ---------------------------

   procedure Onchange_Category_Set
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Translations);
      package HTTP renames Template_Defs.Block_Forum_Category_Set.HTTP;

      P   : constant Parameters.List := Status.Parameters (Request);
      TID : constant Database.Id :=
              V2P.Context.Counter.Get_Value
                (Context => Context.all,
                 Name    => Template_Defs.Set_Global.TID);
      CID : constant Database.Id :=
              Database.Id'Value
                (Parameters.Get (P, HTTP.bfcs_forum_category_set));
   begin
      Database.Set_Category (TID, CID);
   end Onchange_Category_Set;

   ---------------------------
   -- Onchange_Filter_Forum --
   ---------------------------

   procedure Onchange_Filter_Forum
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Filter.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bff_forum_filter_set);
   begin
      --  Keep the sorting scheme into the session

      Context.Set_Value (Template_Defs.Set_Global.FILTER, Filter);

      Context.Remove (Template_Defs.Set_Global.PREVIOUS_FILTER);

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));
   end Onchange_Filter_Forum;

   -------------------------------------
   -- Onchange_Filter_Forum_Page_Size --
   -------------------------------------

   procedure Onchange_Filter_Forum_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Filter_Page_Size.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String          :=
                 Parameters.Get (P, HTTP.bffps_forum_filter_pagesize);
   begin
      --  Keep the sorting scheme into the session

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE,
         Value   => Positive'Value (Filter));

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));
   end Onchange_Filter_Forum_Page_Size;

   ------------------------------------------------
   -- Onchange_Filter_Forum_Page_Size_Preference --
   ------------------------------------------------

   procedure Onchange_Filter_Forum_Page_Size_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames
        Template_Defs.Block_Pref_Forum_Filter_Page_Size.HTTP;

      SID    : constant Session.Id := Status.Session (Request);
      Login  : constant String :=
                 Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bpffps_forum_filter_pagesize_set);
   begin
      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE,
         Value   => Positive'Value (Filter));

      Database.Set_Filter_Page_Size_Preferences
        (Login, Positive'Value (Filter));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_User_Preferences.FILTER, Filter));
   end Onchange_Filter_Forum_Page_Size_Preference;

   --------------------------------------
   -- Onchange_Filter_Forum_Preference --
   --------------------------------------

   procedure Onchange_Filter_Forum_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Pref_Forum_Filter.HTTP;

      SID    : constant Session.Id := Status.Session (Request);
      Login  : constant String :=
                 Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bpff_forum_filter_set);
   begin
      Context.Set_Value (Template_Defs.Set_Global.FILTER, Filter);

      Database.Set_Filter_Preferences
        (Login, Database.Filter_Mode'Value (Filter));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_User_Preferences.FILTER, Filter));
   end Onchange_Filter_Forum_Preference;

   -------------------------------------
   -- Onchange_Filter_Sort_Preference --
   -------------------------------------

   procedure Onchange_Filter_Sort_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Pref_Forum_Sort.HTTP;

      SID    : constant Session.Id := Status.Session (Request);
      Login  : constant String :=
                 Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bpfs_forum_sort_set);
   begin
      Context.Set_Value (Template_Defs.Set_Global.FORUM_SORT, Filter);

      Database.Set_Filter_Sort_Preferences
        (Login, Database.Forum_Sort'Value (Filter));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_User_Preferences.FILTER, Filter));
   end Onchange_Filter_Sort_Preference;

   -------------------------
   -- Onchange_Forum_List --
   -------------------------

   procedure Onchange_Forum_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Chunk_Forum_List_Select.HTTP;
      P   : constant Parameters.List := Status.Parameters (Request);
      Fid : constant Database.Id :=
              Database.Id'Value
                (Parameters.Get (P, HTTP.cfls_sel_forum_list));
   begin
      Templates.Insert (Translations, Database.Get_Categories (Fid));
      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.FID,
         Value   => Fid);
   end Onchange_Forum_List;

   -------------------------
   -- Onchange_Forum_Sort --
   -------------------------

   procedure Onchange_Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Sort.HTTP;
      P    : constant Parameters.List := Status.Parameters (Request);
      Sort : constant String := Parameters.Get (P, HTTP.bfs_forum_sort_set);
   begin
      --  Keep the sorting scheme into the session
      Context.Set_Value (Template_Defs.Set_Global.FORUM_SORT, Sort);

      --  Set the context filter to seven days if the sorting is set to
      --  NEED_ATTENTION.

      if Sort = Template_Defs.Set_Values.Set.NEED_ATTENTION then
         --  Store current filter

         Context.Set_Value
           (Template_Defs.Set_Global.PREVIOUS_FILTER,
            Context.Get_Value (Template_Defs.Set_Global.FILTER));

         Context.Set_Value
           (Template_Defs.Set_Global.FILTER,
            Template_Defs.Set_Values.Set.SEVEN_DAYS);

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Forum_Filter.FORCE_FILTER,
               Template_Defs.Set_Values.Set.SEVEN_DAYS));

      elsif Context.Exist (Template_Defs.Set_Global.PREVIOUS_FILTER) then
         --  Restore previous filter if found

         declare
            P_Filter : constant String :=
                         Context.Get_Value
                           (Template_Defs.Set_Global.PREVIOUS_FILTER);
         begin
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.R_Block_Forum_Filter.FORCE_FILTER, P_Filter));

            Context.Set_Value (Template_Defs.Set_Global.FILTER, P_Filter);

            Context.Remove (Template_Defs.Set_Global.PREVIOUS_FILTER);
         end;
      end if;

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));

      --  Reset FROM

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => 1);
   end Onchange_Forum_Sort;

   ------------------------------------
   -- Onchange_Image_Size_Preference --
   ------------------------------------

   procedure Onchange_Image_Size_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Pref_Image_Size.HTTP;

      SID   : constant Session.Id := Status.Session (Request);
      Login : constant String :=
                Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      P     : constant Parameters.List := Status.Parameters (Request);
      Size  : constant String :=
                Parameters.Get (P, HTTP.bpis_image_size);
   begin
      Morzhol.Logs.Write
        (Name    => Module,
         Content => Size);

      Context.Set_Value (Template_Defs.Set_Global.PREF_IMAGE_SIZE, Size);

      Morzhol.Logs.Write
        (Name    => Module,
         Content => "CONTENT IS " & Size);

      Database.Set_Image_Size_Preferences
        (Login, Database.Image_Size'Value (Size));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_User_Preferences.FILTER, Size));
   end Onchange_Image_Size_Preference;

   --------------------------------
   -- Onclick_CdC_Goto_Next_Page --
   --------------------------------

   procedure Onclick_CdC_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, Settings.Number_CdC_Listed);
   end Onclick_CdC_Goto_Next_Page;

   ------------------------------------
   -- Onclick_CdC_Goto_Previous_Page --
   ------------------------------------

   procedure Onclick_CdC_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, -Settings.Number_CdC_Listed);
   end Onclick_CdC_Goto_Previous_Page;

   --------------------------------
   -- Onclick_CSS_URL_Preference --
   --------------------------------

   procedure Onclick_CSS_URL_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Pref_Css_Url.HTTP;

      SID   : constant Session.Id := Status.Session (Request);
      Login : constant String :=
                Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      P     : constant Parameters.List := Status.Parameters (Request);
      URL   : constant String :=
                Parameters.Get (P, HTTP.bpcu_css_url_set);
   begin
      Context.Set_Value (Template_Defs.Set_Global.CSS_URL, URL);

      Database.Set_CSS_URL_Preferences (Login, URL);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_User_Preferences.FILTER, URL));
   end Onclick_CSS_URL_Preference;

   ----------------------------
   -- Onclick_Goto_Next_Page --
   ----------------------------

   procedure Onclick_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use Template_Defs;
   begin
      Navigation_Links.Goto_Next_Previous
        (Context,
         V2P.Context.Not_Null_Counter.Get_Value
           (Context => Context.all,
            Name    => Set_Global.FILTER_PAGE_SIZE));

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));
   end Onclick_Goto_Next_Page;

   --------------------------------
   -- Onclick_Goto_Previous_Page --
   --------------------------------

   procedure Onclick_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use Template_Defs;
   begin
      Navigation_Links.Goto_Next_Previous
        (Context,
         -V2P.Context.Not_Null_Counter.Get_Value
           (Context => Context.all,
            Name    => Set_Global.FILTER_PAGE_SIZE));

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => Database.Empty_Id));
   end Onclick_Goto_Previous_Page;

   ----------------------------------
   -- Onclick_Hidden_Status_Toggle --
   ----------------------------------

   procedure Onclick_Hidden_Status_Toggle
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      TID : constant Database.Id := V2P.Context.Counter.Get_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.TID);
   begin
      Templates.Insert (Translations, Database.Toggle_Hidden_Status (TID));
   end Onclick_Hidden_Status_Toggle;

   ---------------------------------------------
   -- Onclick_Pref_Private_Message_Preference --
   ---------------------------------------------

   procedure Onclick_Pref_Private_Message_Preference
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Pref_Private_Message.HTTP;

      Login  : constant String :=
                 Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      P      : constant Parameters.List := Status.Parameters (Request);
      Status : constant Boolean := Parameters.Get (P, HTTP.bppm_check) = "on";
   begin
      Database.Set_Private_Message_Preferences (Login, Status);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.R_Block_Pref_Private_Message.SELECTED, Status));
   end Onclick_Pref_Private_Message_Preference;

   ----------------------------
   -- Onclick_Send_Reminders --
   ----------------------------

   procedure Onclick_Send_Reminders
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;
      P    : constant Parameters.List := Status.Parameters (Request);
      User : constant String :=
               Parameters.Get (P, Block_Users_To_Validate.Set.USER);
      Set  : Templates.Translate_Set;
   begin
      --  Only admins can call this routine
      if Context.Exist (Template_Defs.Set_Global.ADMIN) then
         if User = "" then
            Set := Database.Registration.Send_Reminder;
         else
            Set := Database.Registration.Send_Reminder (User);
         end if;
         Templates.Insert (Translations, Set);
      end if;
   end Onclick_Send_Reminders;

   --------------------------------------------
   -- Onclick_User_Photo_List_Goto_Next_Page --
   --------------------------------------------

   procedure Onclick_User_Photo_List_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, Settings.Number_Latest_User_Posts);
   end Onclick_User_Photo_List_Goto_Next_Page;

   ------------------------------------------------
   -- Onclick_User_Photo_List_Goto_Previous_Page --
   ------------------------------------------------

   procedure Onclick_User_Photo_List_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, -Settings.Number_Latest_User_Posts);
   end Onclick_User_Photo_List_Goto_Previous_Page;

   ----------------------------------
   -- Onclick_Users_Goto_Next_Page --
   ----------------------------------

   procedure Onclick_Users_Goto_Next_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, Settings.Number_Users_Listed);
   end Onclick_Users_Goto_Next_Page;

   --------------------------------------
   -- Onclick_Users_Goto_Previous_Page --
   --------------------------------------

   procedure Onclick_Users_Goto_Previous_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Navigation_Links.Goto_Next_Previous
        (Context, -Settings.Number_Users_Listed);
   end Onclick_Users_Goto_Previous_Page;

   --------------------------------------
   -- Onclick_Users_Sort_Registered_On --
   --------------------------------------

   procedure Onclick_Users_Sort_Last_Connected
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      User_Sort (Request, Context, Translations, Database.Last_Connected);
   end Onclick_Users_Sort_Last_Connected;

   -------------------------------
   -- Onclick_Users_Sort_Nb_CdC --
   -------------------------------

   procedure Onclick_Users_Sort_Nb_CdC
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      User_Sort (Request, Context, Translations, Database.Nb_CdC);
   end Onclick_Users_Sort_Nb_CdC;

   ------------------------------------
   -- Onclick_Users_Sort_Nb_Comments --
   ------------------------------------

   procedure Onclick_Users_Sort_Nb_Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      User_Sort (Request, Context, Translations, Database.Nb_Comments);
   end Onclick_Users_Sort_Nb_Comments;

   ----------------------------------
   -- Onclick_Users_Sort_Nb_Photos --
   ----------------------------------

   procedure Onclick_Users_Sort_Nb_Photos
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      User_Sort (Request, Context, Translations, Database.Nb_Photos);
   end Onclick_Users_Sort_Nb_Photos;

   --------------------------------------
   -- Onclick_Users_Sort_Registered_On --
   --------------------------------------

   procedure Onclick_Users_Sort_Registered_On
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      User_Sort (Request, Context, Translations, Database.Date_Created);
   end Onclick_Users_Sort_Registered_On;

   -----------------------------
   -- Onclick_Vote_Week_Photo --
   -----------------------------

   procedure Onclick_Vote_Week_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
      Login : constant String :=
                Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      TID   : constant Database.Id :=
                V2P.Context.Counter.Get_Value
                  (Context => Context.all,
                   Name    => Template_Defs.Set_Global.TID);
   begin
      Database.Toggle_Vote_Week_Photo (Login, TID);
   end Onclick_Vote_Week_Photo;

   ---------------------------------
   -- Onsubmit_Comment_Form_Enter --
   ---------------------------------

   procedure Onsubmit_Comment_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;

      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String :=
                       Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Anonymous    : constant String :=
                       Parameters.Get
                         (P, Block_New_Comment.HTTP.ANONYMOUS_USER);
      Comment      : constant String :=
                       Parameters.Get (P,
                                       Block_New_Comment.
                                         HTTP.global_comment_input);
      Preview_Only : constant Boolean :=
                       Parameters.Get
                         (P, Block_New_Comment.HTTP.REGISTER_COMMENT) = "";
      Comment_Type : constant String :=
                       Parameters.Get (P,
                                       Block_New_Comment.
                                         HTTP.bnc_comment_type);
      Get_Photo_ID : constant String :=
                       Parameters.Get
                         (P, Block_New_Comment.HTTP.bnc_comment_pid);
      Last_Comment : constant String :=
                       Context.Get_Value (Set_Global.CONTEXT_LAST_COMMENT);
      TID          : constant Database.Id :=
                       Database.Id'Value
                         (Parameters.Get (P, Block_New_Comment.HTTP.TID));

      Forum_Type   : V2P.Database.Forum_Type := V2P.Database.Forum_Text;
      Photo_Id     : V2P.Database.Id := V2P.Database.Empty_Id;

      function Is_Valid_Comment (Comment : in String) return Boolean;
      --  Check if the comment is valid

      function To_HTML (Comment : String) return String;
      --  Returns an HTML formatted comment

      ----------------------
      -- Is_Valid_Comment --
      ----------------------

      function Is_Valid_Comment (Comment : in String) return Boolean is
         Is_Valid : Boolean := True;
      begin
         if Comment = "" then
            --  Does not accept empty comment
            Is_Valid := False;
         end if;

         return Is_Valid;
      end Is_Valid_Comment;

      -------------
      -- To_HTML --
      -------------

      function To_HTML (Comment : String) return String is

         function Lf_To_BR (Source : String) return String;
         --  Convert line feed to <br/>

         --------------
         -- Lf_To_BR --
         --------------

         function Lf_To_BR (Source : String) return String is
            Result : Unbounded_String;
            Last   : Integer := Source'First;
         begin
            for K in Source'Range loop
               if Source (K) = ASCII.LF and K > Source'First then
                  Append (Result, Source (Last .. K - 1) & "<br/>");
                  Last := K + 1;
               end if;
            end loop;
            if Last < Source'Last then
               Append (Result, Source (Last .. Source'Last));
            end if;
            return To_String (Result);
         end Lf_To_BR;

      begin
         if Comment_Type = "wiki" then
            --  Always append a line feed (fixes diouzhtu
            --  block formatting)
            return V2P.Wiki.Wiki_To_HTML (Comment & ASCII.LF);
         else
            return Lf_To_BR (Morzhol.Strings.HTML_To_Text (Comment));
         end if;
      end To_HTML;

   begin
      if Get_Photo_ID /= "" then
         Convert_Photo_Id : begin
            Photo_Id := V2P.Database.Id'Value (Get_Photo_ID);
         exception
            when Constraint_Error =>
               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (R_Block_Comment_Form_Enter.ERROR,
                     "Get_photo_ID is " & Get_Photo_ID));
         end Convert_Photo_Id;
      else
         Photo_Id := Database.Empty_Id;
      end if;

      if Parameters.Get (P, Block_New_Comment.HTTP.forum_photo) /= "" then
         Forum_Type := V2P.Database.Forum_Photo;
      end if;

      if Login = "" and then Anonymous = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR, "ERROR_NO_LOGIN"));

      elsif Last_Comment = Comment & '@' & Database.To_String (TID) then
         --   This is a duplicated post

         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR_DUPLICATED, "ERROR"));

      elsif TID /= Database.Empty_Id
        and then not Is_Valid_Comment (Comment)
      then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR, "ERROR"));
         --  ??? Adds an error message

      else

         Save_Or_Preview : declare
            HTML_Comment : constant String := To_HTML (Comment);
         begin

            if Preview_Only then
               Templates.Insert
                 (Translations,
                  Templates.Assoc (R_Block_Comment_Form_Enter.
                      CAN_SUBMIT_COMMENT, "true"));
               Templates.Insert
                 (Translations,
                  Templates.Assoc (R_Block_Comment_Form_Enter.
                      HTML_COMMENT_TO_CHECK, HTML_Comment));
            else
               Insert_Comment : declare
                  Cid : constant Database.Id := Database.Insert_Comment
                    (Uid       => Login,
                     Anonymous => Anonymous,
                     Thread    => TID,
                     Name      => "",
                     Comment   => HTML_Comment,
                     Pid       => Photo_Id);
               begin
                  --  Adds the new comment in context to
                  --  prevent duplicated post

                  Context.Set_Value
                    (Set_Global.CONTEXT_LAST_COMMENT,
                     Comment & '@' & Database.To_String (TID));

                  Templates.Insert
                    (Translations,
                     Database.Get_Comment
                       (Cid,
                        TZ =>
                          Context.Get_Value (Template_Defs.Set_Global.TZ)));

                  Templates.Insert
                    (Translations,
                     Database.Get_Forum
                       (V2P.Context.Counter.Get_Value
                          (Context => Context.all,
                           Name    => Set_Global.FID),
                        Tid => TID));

                  Templates.Insert
                    (Translations,
                     Database.Get_Post
                       (Tid        => V2P.Context.Counter.Get_Value
                          (Context => Context.all,
                           Name    => Set_Global.TID),
                        Forum_Type => Forum_Type,
                        TZ         =>
                          Context.Get_Value (Template_Defs.Set_Global.TZ),
                        Admin      => False));
               end Insert_Comment;
            end if;
         end Save_Or_Preview;
      end if;
   end Onsubmit_Comment_Form_Enter;

   ----------------------------------
   -- Onsubmit_Metadata_Form_Enter --
   ----------------------------------

   procedure Onsubmit_Metadata_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Translations);
      use Image.Metadata.Geographic;

      function Get (Parameter_Name : in String) return Geo_Coordinate;
      --  Returns the given parameter or zero if not found

      P : constant Parameters.List := Status.Parameters (Request);

      ---------
      -- Get --
      ---------

      function Get (Parameter_Name : in String) return Geo_Coordinate is
         Param : constant String := Parameters.Get (P, Parameter_Name);
         Coordinate : Geo_Coordinate := 0.0;
      begin
         if Param /= "" then
            Coordinate := Geo_Coordinate'Value (Param);
         end if;
         return Coordinate;
      end Get;

   begin

      Morzhol.Logs.Write
        (Name    => Module,
         Content =>
           "(Onsubmit_Metadata_Form_Enter) : Get Metadata "
            & Parameters.Get (P, Template_Defs.Block_Metadata.HTTP.bm_latitude)
            & " " & Parameters.Get
              (P, Template_Defs.Block_Metadata.HTTP.bm_longitude));

      Insert_Metadata : declare
         Latitude_Coord      : constant Geo_Coordinate := Get
           (Template_Defs.Block_Metadata.HTTP.bm_latitude);
         Longitude_Coord     : constant Geo_Coordinate := Get
           (Template_Defs.Block_Metadata.HTTP.bm_longitude);
         Latitude_Position   : Latitude;
         Longitude_Postition : Longitude;
      begin
         if Latitude_Coord = 0.0 or else Longitude_Coord = 0.0 then
            Context.Set_Value
              (V2P.Template_Defs.Set_Global.ERROR_METADATA_NULL_METADATA,
               "ERROR");

         elsif not Context.Exist (Template_Defs.Set_Global.TID) then
            Context.Set_Value
              (V2P.Template_Defs.Set_Global.ERROR_METADATA_UNKNOWN_PHOTO,
               "ERROR");

         else
            Latitude_Position.Format (Latitude_Coord);
            Longitude_Postition.Format (Longitude_Coord);

            Database.Insert_Metadata
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.TID),
               Float (Latitude_Coord),
               Float (Longitude_Coord),
               Image.Metadata.Geographic.Image (Latitude_Position),
               Image.Metadata.Geographic.Image (Longitude_Postition));
         end if;
      end Insert_Metadata;

   exception
      when Constraint_Error =>
         Morzhol.Logs.Write
           (Name    => Module,
            Content =>
            "(Onsubmit_Metadata_Form_Enter) : Constraint Error with "
            & Parameters.Get (P, Template_Defs.Block_Metadata.HTTP.bm_latitude)
            & " " & Parameters.Get
              (P, Template_Defs.Block_Metadata.HTTP.bm_longitude));
         Context.Set_Value
           (V2P.Template_Defs.Set_Global.ERROR_METADATA_WRONG_METADATA,
            "ERROR");
   end Onsubmit_Metadata_Form_Enter;

   --------------------------------
   -- Onsubmit_Plp_Lost_Password --
   --------------------------------

   procedure Onsubmit_Plp_Lost_Password
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use Template_Defs;
      P        : constant Parameters.List := Status.Parameters (Request);
      Email    : constant String :=
                   Parameters.Get
                     (P, Page_Lost_Password.HTTP.USER_EMAIL);
      Password : constant String :=
                   Database.Get_Password_From_Email (Email);
   begin
      if Email = "" or else Password = "" then
         --  Display error message
         Templates.Insert
           (Translations, Templates.Assoc (R_Page_Lost_Password.ERROR, True));
         return;
      end if;

      --  Send the e-mail with the password

      V2P.Email.Send_Lost_Password (Email, Password, Email);

   exception
      when others =>
         Morzhol.Logs.Write
           (Name    => Module,
            Content =>
            "(Onsubmit_Plp_Lost_Email) : sending e-mail failed for email "
            & Email & ", password " & Password,
            Kind    => Morzhol.Logs.Error);
         Templates.Insert
           (Translations,
            Templates.Assoc (R_Page_Lost_Password.ERROR, True));
   end Onsubmit_Plp_Lost_Password;

   ------------------------------
   -- Onsubmit_Post_Form_Enter --
   ------------------------------

   procedure Onsubmit_Post_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;

      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String :=
                       Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Name         : constant String :=
                       Parameters.Get (P, Page_Forum_New_Text_Entry.HTTP.NAME);
      Comment      : constant String :=
                       Parameters.Get
                         (P, Page_Forum_New_Text_Entry.HTTP.comment_input);
      Category     : constant String :=
                       Parameters.Get
                         (P, Chunk_Forum_List_Select.HTTP.CATEGORY);
      Last_Name    : constant String :=
                       Context.Get_Value (Set_Global.CONTEXT_LAST_POST_NAME);
      Comment_Wiki : constant String := V2P.Wiki.Wiki_To_HTML (Comment);
      PID          : Database.Id := 0;
   begin
      Convert_PID : declare
         --  Check PID which could be empty if posting on a text forum
         PID_Str : constant String :=
                     Parameters.Get (P, Page_Forum_New_Photo_Entry.HTTP.PID);
      begin
         if PID_Str /= "" then
            PID := Database.Id'Value (PID_Str);
         end if;
      end Convert_PID;

      --  Check for empty fields

      if Login = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               Template_Defs.R_Block_Post_Form_Enter.Set.ERROR_LOGIN));

      elsif Category = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               Template_Defs.R_Block_Post_Form_Enter.Set.ERROR_CATEGORY));

      elsif Name = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               Template_Defs.R_Block_Post_Form_Enter.Set.ERROR_TITLE));

      elsif Last_Name = Name then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               Template_Defs.R_Block_Post_Form_Enter.Set.ERROR_DUPLICATED));

      else
         Insert_Post : declare
            CID     : constant Database.Id := Database.Id'Value (Category);
            Post_Id : constant Database.Id :=
                        Database.Insert_Post
                          (Uid         => Login,
                           Category_Id => CID,
                           Name        => Name,
                           Comment     => Comment_Wiki,
                           Pid         => PID);
         begin
            if Post_Id /= Database.Empty_Id then
               --  Set new context TID (needed by
               --  Onsubmit_Metadata_Form_Enter_Callback)

               V2P.Context.Counter.Set_Value
                 (Context => Context.all,
                  Name    => Set_Global.TID,
                  Value   => Post_Id);

               Context.Set_Value (Set_Global.CONTEXT_LAST_POST_NAME, Name);

               if Context.Exist (Set_Global.FID) then
                  Context.Remove (Set_Global.FID);
               end if;

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (R_Block_Post_Form_Enter.URL,
                     Page_Forum_Entry.Set.URL & '?' &
                     Page_Forum_Entry.HTTP.TID & '='
                     & Database.To_String (Post_Id)));

            else
               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (R_Block_Post_Form_Enter.ERROR,
                     "DATABASE INSERT FAILED"));
            end if;
         end Insert_Post;
      end if;

      if PID /= Database.Empty_Id
        and then Context.Exist (Set_Global.TID)
      then
         Onsubmit_Metadata_Form_Enter (Request, Context, Translations);
      end if;
   end Onsubmit_Post_Form_Enter;

   ------------------------------
   -- Onsubmit_Private_Message --
   ------------------------------

   procedure Onsubmit_Private_Message
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;
      P         : constant Parameters.List := Status.Parameters (Request);
      Message   : constant String :=
                    Parameters.Get
                      (P,
                       Block_Private_Message.HTTP.bpm_private_message_input);
      User_Name : constant String :=
                    Parameters.Get
                      (P, Block_Private_Message.HTTP.bpm_user_name);
   begin
      Send_Mail : declare
         Login     : constant String :=
                       Context.Get_Value (Set_Global.LOGIN);
         User_Data : Database.User_Data;
      begin
         User_Data := Database.Get_User_Data (User_Name);

         Email.Send_Private_Message
           (Login, User_Name, To_String (User_Data.Email), Message);

         Templates.Insert
           (Translations,
            Templates.Assoc (R_Block_Send_Private_Message.ERROR, False));

      exception
         when others =>
            Morzhol.Logs.Write
              (Name    => Module,
               Content =>
               "(Onsubmit_Private_Message) : sending e-mail failed for "
               & Login,
               Kind    => Morzhol.Logs.Error);
            Templates.Insert
              (Translations,
               Templates.Assoc (R_Block_Send_Private_Message.ERROR, True));
      end Send_Mail;
   end Onsubmit_Private_Message;

   --------------------------------
   -- Onsubmit_Pur_Register_User --
   --------------------------------

   procedure Onsubmit_Pur_Register_User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use Template_Defs;
      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String :=
                   Parameters.Get
                     (P, Page_User_Register.HTTP.USER_LOGIN);
      Password : constant String :=
                   Parameters.Get
                     (P, Page_User_Register.HTTP.USER_PASSWORD);
      Email    : constant String :=
                   Parameters.Get
                     (P, Page_User_Register.HTTP.USER_EMAIL);
   begin
      --  Record registration request into the database

      if Login = "" or else Password = "" or else Email = ""
        or else not Database.Register_User (Login, Password, Email)
      then
         --  Display error message on the login page, a single error for now
         --  (duplicate login).
         Templates.Insert
           (Translations, Templates.Assoc (R_Page_User_Register.ERROR, True));
         return;
      end if;

      --  Send the e-mail for confirmation

      V2P.Email.Send_Register_User (Login, Password, Email);

   exception
      when others =>
         Morzhol.Logs.Write
           (Name    => Module,
            Content =>
            "(Onsubmit_Pur_Register_User) : sending e-mail failed for "
            & Login & ", email " & Email & ", password " & Password,
            Kind    => Morzhol.Logs.Error);
         Templates.Insert
           (Translations,
            Templates.Assoc (R_Page_User_Register.ERROR, True));
   end Onsubmit_Pur_Register_User;

   -------------------
   -- Onsubmit_Rate --
   -------------------

   procedure Onsubmit_Rate
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;

      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String :=
                   Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      TID      : constant Database.Id :=
                   Database.Id'Value
                     (Parameters.Get (P, Template_Defs.Set_Global.TID));

      Criteria : constant String :=
                   Parameters.Get
                     (P, Block_New_Vote.Set.AJAX_RATE_CRITERIA);
      Value    : constant String :=
                   Parameters.Get
                     (P, Block_New_Vote.Set.AJAX_RATE_VAL);
   begin
      Database.Update_Rating
        (Uid      => Login,
         Tid      => TID,
         Criteria => Criteria,
         Value    => Value);

      Templates.Insert
        (Translations,
         Templates.Assoc (Block_New_Vote.IN_RATE_CB, "t"));
   end Onsubmit_Rate;

   --------------------------
   -- Onsubmit_Search_Form --
   --------------------------

   procedure Onsubmit_Search_Form
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      P                   : constant Parameters.List :=
                              Status.Parameters (Request);
      Login               : constant String :=
                              Context.Get_Value
                                (Template_Defs.Set_Global.LOGIN);
      User, Post, Comment : Boolean := False;
      S_Split             : String_Split.Slice_Set;
   begin
      --  Check if logged

      if Login = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc (Template_Defs.R_Page_Search.ERROR, True));

      else
         for K in
           1 ..
             Parameters.Count (P, Template_Defs.Page_Search.Set.SNAME)
         loop
            if Parameters.Get (P, Template_Defs.Page_Search.Set.SNAME, K)
              = Template_Defs.Page_Search.Set.SN_USERS
            then
               User := True;
            elsif Parameters.Get (P, Template_Defs.Page_Search.Set.SNAME, K)
              = Template_Defs.Page_Search.Set.SN_POSTS
            then
               Post := True;
            elsif Parameters.Get (P, Template_Defs.Page_Search.Set.SNAME, K)
              = Template_Defs.Page_Search.Set.SN_COMMENTS
            then
               Comment := True;
            end if;
         end loop;

         String_Split.Create
           (S_Split,
            From       =>
              Parameters.Get (P, Template_Defs.Page_Search.HTTP.PATTERN),
            Separators => " ");

         declare
            W_Set : Database.Search.Word_Set
              (1 .. Natural (String_Split.Slice_Count (S_Split)));
            Last  : Natural := 0;
         begin
            --  Get all words from the HTTP.PATTERN entry with more than 2
            --  characters.

            for K in 1 .. String_Split.Slice_Number (W_Set'Last) loop
               declare
                  Word : constant String :=
                           String (String_Split.Slice (S_Split, K));
               begin
                  if Word'Length > 2 then
                     Last := Last + 1;
                     W_Set (Last) := To_Unbounded_String (Word);
                  end if;
               end;
            end loop;

            if Last > 0 then
               if User then
                  Get_Users : declare
                     T       : Templates.Translate_Set;
                     Results : Unbounded_String;
                  begin
                     Templates.Insert (T, Translations);

                     Templates.Insert
                       (T, Database.Search.Users (W_Set (1 .. Last)));
                     Append
                       (Results,
                        Unbounded_String'(Templates.Parse
                          (Template_Defs.Chunk_Search_User.Template, T)));

                     Templates.Insert
                       (Translations,
                        Templates.Assoc
                          (Template_Defs.R_Page_Search.SEARCH_RESULTS_USERS,
                           Results));
                  end Get_Users;
               end if;

               if Comment then
                  Get_Comments : declare
                     T       : Templates.Translate_Set;
                     Results : Unbounded_String;
                  begin
                     Templates.Insert (T, Translations);

                     Templates.Insert
                       (T, Database.Search.Comments (W_Set (1 .. Last)));
                     Append
                       (Results,
                        Unbounded_String'(Templates.Parse
                          (Template_Defs.Chunk_Search_Comment.Template, T)));

                     Templates.Insert
                       (Translations,
                        Templates.Assoc
                          (Template_Defs.R_Page_Search.SEARCH_RESULTS_COMMENTS,
                           Results));
                  end Get_Comments;
               end if;

               if Post then
                  Get_Posts : declare
                     T       : Templates.Translate_Set;
                     Results : Unbounded_String;
                  begin
                     Templates.Insert (T, Translations);

                     Templates.Insert
                       (T, Database.Search.Posts (W_Set (1 .. Last)));
                     Append
                       (Results,
                        Unbounded_String'(Templates.Parse
                          (Template_Defs.Chunk_Search_Post.Template, T)));

                     Templates.Insert
                       (Translations,
                        Templates.Assoc
                          (Template_Defs.R_Page_Search.SEARCH_RESULTS_POSTS,
                           Results));
                  end Get_Posts;

                  Get_Text_Posts : declare
                     T       : Templates.Translate_Set;
                     Results : Unbounded_String;
                  begin
                     Templates.Insert (T, Translations);

                     Templates.Insert
                       (T, Database.Search.Text_Posts (W_Set (1 .. Last)));
                     Append
                       (Results,
                        Unbounded_String'(Templates.Parse
                          (Template_Defs.Chunk_Search_Text_Post.Template, T)));

                     Templates.Insert
                       (Translations,
                        Templates.Assoc
                          (Template_Defs.R_Page_Search.
                             SEARCH_RESULTS_TEXT_POSTS,
                           Results));
                  end Get_Text_Posts;
               end if;

            end if;
         end;
      end if;
   end Onsubmit_Search_Form;

   ----------------------------------------
   -- Onsubmit_User_Page_Edit_Form_Enter --
   ----------------------------------------

   procedure Onsubmit_User_Page_Edit_Form_Enter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Template_Defs;

      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String :=
                       Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Content      : constant String :=
                       Parameters.Get (P, Block_User_Page.HTTP.CONTENT);
      Content_HTML : constant String := V2P.Wiki.Wiki_To_HTML (Content);

   begin
      Database.Update_Page
        (Uid          => Login,
         Content      => Content,
         Content_HTML => Content_HTML);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (R_Block_User_Page_Edit_Form_Enter.USER_PAGE_HTML_CONTENT,
            Content_HTML));
   end Onsubmit_User_Page_Edit_Form_Enter;

   ---------------
   -- User_Sort --
   ---------------

   procedure User_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      Sort_On      : in              Database.User_Sort)
   is
      pragma Unreferenced (Request, Translations);
      use type Database.Order_Direction;
      use type Database.User_Sort;

      Sort  : Database.User_Sort := Database.Last_Connected;
      Order : Database.Order_Direction := Database.DESC;
   begin
      if Context.Exist (Template_Defs.Set_Global.USER_SORT) then
         Sort := Database.User_Sort'Value
           (Context.Get_Value (Template_Defs.Set_Global.USER_SORT));
      end if;

      if Context.Exist (Template_Defs.Set_Global.USER_ORDER) then
         Order := Database.Order_Direction'Value
           (Context.Get_Value (Template_Defs.Set_Global.USER_ORDER));
      end if;

      if Sort = Sort_On then
         --  Invert current order
         if Order = Database.ASC then
            Order := Database.DESC;
         else
            Order := Database.ASC;
         end if;

      else
         Sort := Sort_On;
         Order := Database.DESC;
      end if;

      Context.Set_Value
        (Template_Defs.Set_Global.USER_ORDER,
         Database.Order_Direction'Image (Order));
      Context.Set_Value
        (Template_Defs.Set_Global.USER_SORT,
         Database.User_Sort'Image (Sort));
   end User_Sort;

end V2P.Callbacks.Ajax;
