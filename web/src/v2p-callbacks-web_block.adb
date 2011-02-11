------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

with AWS.Parameters;
with AWS.Utils;

with V2P.Context;
with V2P.Database.Preference;
with V2P.Database.Registration;
with V2P.Database.Vote;
with V2P.Navigation_Links;
with V2P.Settings;
with V2P.URL;

with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Global_Rating;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_Private_Message;
with V2P.Template_Defs.Block_User_Avatar;
with V2P.Template_Defs.Block_Pref_User_Email;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_User_Photo_List;
with V2P.Template_Defs.Block_Vote_Week_Photo;
with V2P.Template_Defs.Chunk_Forum_Category;
with V2P.Template_Defs.Set_Global;

package body V2P.Callbacks.Web_Block is

   procedure User_Post_List
     (Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set;
      Forum        : in     Database.Forum_Filter;
      User_Name    : in     String;
      From         : in     Positive;
      Limit        : in     Positive);

   ---------
   -- CdC --
   ---------

   procedure CdC
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      From : Positive := 1;
   begin
      if Context.Exist (Template_Defs.Set_Global.NAV_FROM) then
         From := V2P.Context.Not_Null_Counter.Get_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.NAV_FROM);
      end if;

      Templates.Insert (Translations, Database.Vote.Get_CdC (From));
   end CdC;

   --------------
   -- CdC_Info --
   --------------

   procedure CdC_Info
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert (Translations, Database.Vote.Get_CdC_Info);
   end CdC_Info;

   --------------
   -- Comments --
   --------------

   procedure Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Templates.Insert
           (Translations,
            Database.Get_Comments
              (Tid   =>  V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.TID),
               Login => Context.Get_Value (Template_Defs.Set_Global.LOGIN),
               TZ    => Context.Get_Value (Template_Defs.Set_Global.TZ)));
      end if;
   end Comments;

   ----------
   -- Exif --
   ----------

   procedure Exif
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Tid : Database.Id;
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Tid := V2P.Context.Counter.Get_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.TID);

         if Tid /= Database.Empty_Id then
            Templates.Insert
              (Translations, Database.Get_Exif (Tid));
         end if;
      end if;
   end Exif;

   ---------------------------
   -- Forum_Category_Filter --
   ---------------------------

   procedure Forum_Category_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.FID) then
         Templates.Insert
           (Translations,
            Database.Get_Categories
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.FID)));
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER_CATEGORY,
            Context.Get_Value (Template_Defs.Set_Global.FILTER_CATEGORY)));
   end Forum_Category_Filter;

   ------------------------
   -- Forum_Category_Set --
   ------------------------

   procedure Forum_Category_Set
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.FID) then
         Templates.Insert
           (Translations,
            Database.Get_Categories
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.FID)));
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Chunk_Forum_Category.CATEGORY_SET, True));
   end Forum_Category_Set;

   ------------------
   -- Forum_Filter --
   ------------------

   procedure Forum_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER,
            Context.Get_Value (Template_Defs.Set_Global.FILTER)));
   end Forum_Filter;

   ----------------------------
   -- Forum_Filter_Page_Size --
   ----------------------------

   procedure Forum_Filter_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER_PAGE_SIZE,
            V2P.Navigation_Links.Context_Page_Size.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE)));
   end Forum_Filter_Page_Size;

   ----------------
   -- Forum_List --
   ----------------

   procedure Forum_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.FID) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_Forum_List.Current_FID,
               V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.FID)));
      end if;

      Templates.Insert
        (Translations,
         Database.Get_Forums
           (Filter => Database.Forum_All,
            TZ     => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Login  => Context.Get_Value (Template_Defs.Set_Global.LOGIN)));
   end Forum_List;

   -----------------------------
   -- Forum_Photo_List_Select --
   -----------------------------

   procedure Forum_Photo_List_Select
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Forums
           (Filter => Database.Forum_Photo,
            TZ     => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Login  => Context.Get_Value (Template_Defs.Set_Global.LOGIN)));
   end Forum_Photo_List_Select;

   ----------------
   -- Forum_Sort --
   ----------------

   procedure Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_SORT,
            Context.Get_Value (Template_Defs.Set_Global.FORUM_SORT)));
   end Forum_Sort;

   ----------------------------
   -- Forum_Text_List_Select --
   ----------------------------

   procedure Forum_Text_List_Select
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Forums
           (Filter => Database.Forum_Text,
            TZ     => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Login  => Context.Get_Value (Template_Defs.Set_Global.LOGIN)));
   end Forum_Text_List_Select;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Page_Size : constant Navigation_Links.Page_Size :=
                    V2P.Navigation_Links.Context_Page_Size.Get_Value
                      (Context => Context.all,
                       Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE);
      Nav_From  : constant Positive :=
                    V2P.Context.Not_Null_Counter.Get_Value
                      (Context => Context.all,
                       Name    => Template_Defs.Set_Global.NAV_FROM);
   begin
      Navigation_Links.Get_Threads
        (Context, Page_Size, Nav_From, Translations);
   end Forum_Threads;

   -------------------
   -- Global_Rating --
   -------------------

   procedure Global_Rating
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      TID : constant Database.Id :=
              V2P.Context.Counter.Get_Value
                (Context => Context.all,
                 Name    => Template_Defs.Set_Global.TID);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Templates.Insert
           (Translations,
            Database.Vote.Get_Global_Rating (TID));
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_Global_Rating.REVEALED,
               Database.Is_Revealed (TID)));
      end if;
   end Global_Rating;

   ------------------
   -- Latest_Posts --
   ------------------

   procedure Latest_Posts
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Latest_Posts
           (Settings.Number_Latest_Posts,
            TZ         => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Photo_Only => True));
   end Latest_Posts;

   ------------------
   -- Latest_Users --
   ------------------

   procedure Latest_Users
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Latest_Users (Settings.Number_Latest_Users));
   end Latest_Users;

   --------------
   -- Metadata --
   --------------

   procedure Metadata
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Tid : Database.Id;
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         if Context.Exist
           (V2P.Template_Defs.Set_Global.ERROR_METADATA_NULL_METADATA) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (V2P.Template_Defs.Set_Global.ERROR_METADATA_NULL_METADATA,
                  "ERROR"));
            Context.Remove
              (V2P.Template_Defs.Set_Global.ERROR_METADATA_NULL_METADATA);

         elsif Context.Exist
           (V2P.Template_Defs.Set_Global.ERROR_METADATA_UNKNOWN_PHOTO) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (V2P.Template_Defs.Set_Global.ERROR_METADATA_UNKNOWN_PHOTO,
                  "ERROR"));
            Context.Remove
              (V2P.Template_Defs.Set_Global.ERROR_METADATA_UNKNOWN_PHOTO);

         elsif Context.Exist
           (V2P.Template_Defs.Set_Global.ERROR_METADATA_WRONG_METADATA) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (V2P.Template_Defs.Set_Global.ERROR_METADATA_WRONG_METADATA,
                  "ERROR"));
            Context.Remove
              (V2P.Template_Defs.Set_Global.ERROR_METADATA_WRONG_METADATA);

         else
            Tid := V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID);
            if Tid /= Database.Empty_Id then
               Templates.Insert
                 (Translations, Database.Get_Metadata (Tid));
            end if;
         end if;
      end if;
   end Metadata;

   -----------------
   -- New_Comment --
   -----------------

   procedure New_Comment
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use type Database.Forum_Type;
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Block_New_Comment.FORUM_FOR_PHOTO,
               Database.Get_Forum_Type
                 (V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID))
               = Database.Forum_Photo));
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Set_Global.TID,
               (V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID))));
      end if;
   end New_Comment;

   --------------
   -- New_Post --
   --------------

   procedure New_Post
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist ("TID") then
         Context.Remove ("TID");
      end if;
   end New_Post;

   --------------
   -- New_Vote --
   --------------

   procedure New_Vote
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use AWS.Templates;

      Ratings : Templates.Tag;

   begin
      if Context.Exist (Template_Defs.Set_Global.FID) then
         Templates.Insert
           (Translations,
            Database.Get_Categories
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.FID)));
      end if;

      if Context.Exist (Template_Defs.Set_Global.TID) then
         if Context.Exist (Template_Defs.Set_Global.LOGIN) then
            Templates.Insert
              (Translations,
               Database.Vote.Get_User_Rating_On_Post
                 (Uid => Context.Get_Value (Template_Defs.Set_Global.LOGIN),
                  Tid => V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID)));
         end if;
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Set_Global.TID,
               (V2P.Context.Counter.Get_Value
                  (Context => Context.all,
                   Name    => Template_Defs.Set_Global.TID))));
      end if;

      Ratings := Ratings & "1" & "2" & "3" & "4" & "5";

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Block_New_Vote.RATING, Ratings));
   end New_Vote;

   -----------------------
   -- Photo_Of_The_Week --
   -----------------------

   procedure Photo_Of_The_Week
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert (Translations, Database.Vote.Get_Photo_Of_The_Week);
   end Photo_Of_The_Week;

   ------------------
   -- Pref_CSS_URL --
   ------------------

   procedure Pref_CSS_URL
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Preferences : Database.User_Settings;
   begin
      if Login = "" then
         Preferences := Database.Default_User_Settings;
      else
         Database.Preference.User (Login, Preferences);
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.CSS_URL,
            Ada.Strings.Unbounded.To_String (Preferences.CSS_URL)));
   end Pref_CSS_URL;

   -----------------------
   -- Pref_Forum_Filter --
   -----------------------

   procedure Pref_Forum_Filter
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Preferences : Database.User_Settings;
   begin
      Database.Preference.User (Login, Preferences);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER,
            Database.Filter_Mode'Image (Preferences.Filter)));
   end Pref_Forum_Filter;

   ---------------------------------
   -- Pref_Forum_Filter_Page_Size --
   ---------------------------------

   procedure Pref_Forum_Filter_Page_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Preferences : Database.User_Settings;
   begin
      Database.Preference.User (Login, Preferences);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER_PAGE_SIZE,
            Utils.Image (Preferences.Page_Size)));
   end Pref_Forum_Filter_Page_Size;

   ---------------------
   -- Pref_Forum_Sort --
   ---------------------

   procedure Pref_Forum_Sort
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Preferences : Database.User_Settings;
   begin
      Database.Preference.User (Login, Preferences);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_SORT,
            Database.Forum_Sort'Image (Preferences.Sort)));
   end Pref_Forum_Sort;

   ---------------------
   -- Pref_Image_Size --
   ---------------------

   procedure Pref_Image_Size
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Preferences : Database.User_Settings;
   begin
      if Login = "" then
         Preferences := Database.Default_User_Settings;
      else
         Database.Preference.User (Login, Preferences);
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.PREF_IMAGE_SIZE,
            Database.Image_Size'Image (Preferences.Image_Size)));
   end Pref_Image_Size;

   ---------------------
   -- Pref_User_Email --
   ---------------------

   procedure Pref_User_Email
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Block_Pref_User_Email.USER_EMAIL,
            Database.Get_User_Data (Uid => User_Name).Email));
   end Pref_User_Email;

   ---------------------
   -- Private_Message --
   ---------------------

   procedure Private_Message
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI         : constant String := Status.URI (Request);
      User_Name   : constant String := URL.User_Name (URI);
      Preferences : Database.User_Settings;
   begin
      Database.Preference.User (User_Name, Preferences);

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Block_Private_Message.ACCEPT_PRIVATE_MESSAGE,
            Preferences.Accept_Private_Message));
   end Private_Message;

   -----------------
   -- Quick_Login --
   -----------------

   procedure Quick_Login
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.LOGIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Set_Global.LOGIN,
               String'(Context.Get_Value  (Template_Defs.Set_Global.LOGIN))));
      end if;
   end Quick_Login;

   -----------------
   -- User_Avatar --
   -----------------

   procedure User_Avatar
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Ada;
      pragma Unreferenced (Context);

      function Get_User_Name return String;
      --  This procedure can be called either from the user's page or with a
      --  parameter LOGIN.

      -------------------
      -- Get_User_Name --
      -------------------

      function Get_User_Name return String is
         P         : constant Parameters.List := Status.Parameters (Request);
         Login     : constant String :=
                       Parameters.Get (P, Template_Defs.Set_Global.LOGIN);
         URI       : constant String := Status.URI (Request);
         User_Name : constant String := URL.User_Name (URI);
      begin
         if Login = "" then
            return User_Name;
         else
            return Login;
         end if;
      end Get_User_Name;

      User_Name : constant String := Get_User_Name;

      Preferences : Database.User_Settings;
   begin
      if User_Name /= "" then
         Database.Preference.User (User_Name, Preferences);

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_User_Avatar.USER_AVATAR,
               Strings.Unbounded.To_String (Preferences.Avatar)));
      end if;
   end User_Avatar;

   -----------------------
   -- User_Comment_List --
   -----------------------

   procedure User_Comment_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User_Comment
           (Uid => User_Name, Limit => 50, Textify => True));
   end User_Comment_List;

   -----------------------
   -- User_Message_List --
   -----------------------

   procedure User_Message_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      User_Post_List
        (Context, Translations, Database.Forum_Text,
         User_Name => User_Name,
         From      => 1,
         Limit     => Settings.Number_Latest_User_Messages);
   end User_Message_List;

   ---------------
   -- User_Page --
   ---------------

   procedure User_Page
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations, Database.Get_User_Page (Uid => User_Name));

      --  We enter the user's page, start navigate starting with first photo

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => 1);

      --  Set User_Name

      Context.Set_Value (Template_Defs.Block_User_Page.USER_NAME, User_Name);
   end User_Page;

   ---------------------
   -- User_Photo_List --
   ---------------------

   procedure User_Photo_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      From : Positive := 1;
   begin
      if Context.Exist (Template_Defs.Set_Global.NAV_FROM) then
         From := V2P.Context.Not_Null_Counter.Get_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.NAV_FROM);
      end if;

      User_Post_List
        (Context, Translations, Database.Forum_Photo,
         User_Name => Context.Get_Value
           (Template_Defs.Block_User_Page.USER_NAME),
         From      => From,
         Limit     => Settings.Number_Latest_User_Posts);
   end User_Photo_List;

   --------------------
   -- User_Post_List --
   --------------------

   procedure User_Post_List
     (Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set;
      Forum        : in     Database.Forum_Filter;
      User_Name    : in     String;
      From         : in     Positive;
      Limit        : in     Positive)
   is
      Admin      : constant Boolean :=
                     Context.Exist (Template_Defs.Set_Global.ADMIN)
                   and then Context.Get_Value
                     (Template_Defs.Set_Global.ADMIN) = "TRUE";
      Set        : Templates.Translate_Set;
      Navigation : Navigation_Links.Post_Ids.Vector;
      Nb_Lines   : Natural;
      Total      : Natural;
      L_From     : Positive := From;
   begin
      Database.Get_Threads
        (User          => User_Name,
         Navigation    => Navigation,
         Set           => Set,
         Admin         => Admin,
         From          => L_From,
         Nb_Lines      => Nb_Lines,
         Total_Lines   => Total,
         Forum         => Forum,
         Only_Revealed => True,
         Page_Size     => Limit,
         TZ            => Context.Get_Value (Template_Defs.Set_Global.TZ));

      Templates.Insert (Translations, Set);

      --  Set IS_USER_PAGE template boolean
      --  This avoid to set alarm on restrictive filter on user page

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.IS_USER_PAGE, "True"));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Block_User_Photo_List.USER_NAME, User_Name));

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_RETURNED,
         Value   => Nb_Lines);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_TOTAL,
         Value   => Total);
   end User_Post_List;

   ----------------
   -- User_Stats --
   ----------------

   procedure User_Stats
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User_Stats
           (User_Name, Context.Get_Value (Template_Defs.Set_Global.TZ)));
   end User_Stats;

   ----------------------------
   -- User_Voted_Photos_List --
   ----------------------------

   procedure User_Voted_Photos_List
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
      Set       : Templates.Translate_Set;
   begin
      Set := Database.Vote.Get_User_Voted_Photos (User_Name);
      Templates.Insert (Translations, Set);
   end User_Voted_Photos_List;

   -----------
   -- Users --
   -----------

   procedure Users
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      From  : Positive := 1;
      Sort  : Database.User_Sort := Database.Last_Connected;
      Order : Database.Order_Direction := Database.DESC;
   begin
      if Context.Exist (Template_Defs.Set_Global.NAV_FROM) then
         From := V2P.Context.Not_Null_Counter.Get_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.NAV_FROM);
      end if;

      if Context.Exist (Template_Defs.Set_Global.USER_SORT) then
         Sort := Database.User_Sort'Value
           (Context.Get_Value (Template_Defs.Set_Global.USER_SORT));
      end if;

      if Context.Exist (Template_Defs.Set_Global.USER_ORDER) then
         Order := Database.Order_Direction'Value
           (Context.Get_Value (Template_Defs.Set_Global.USER_ORDER));
      end if;

      Templates.Insert
        (Translations,
         Database.Get_Users
           (From, Sort, Order,
            Context.Get_Value (Template_Defs.Set_Global.TZ)));

      --  Set translations values

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.USER_ORDER,
            Database.Order_Direction'Image (Order)));
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.USER_SORT,
            Database.User_Sort'Image (Sort)));
   end Users;

   -----------------------
   -- Users_To_Validate --
   -----------------------

   procedure Users_To_Validate
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations, Database.Registration.Users_To_Validate);
   end Users_To_Validate;

   ---------------------
   -- Vote_Week_Photo --
   ---------------------

   procedure Vote_Week_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.LOGIN)
        and then Context.Exist (Template_Defs.Set_Global.TID)
      then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_Vote_Week_Photo.HAS_USER_VOTE,
               Database.Vote.Has_User_Vote
                 (Uid => Context.Get_Value (Template_Defs.Set_Global.LOGIN),
                  Tid => V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID))));
      end if;
   end Vote_Week_Photo;

end V2P.Callbacks.Web_Block;
