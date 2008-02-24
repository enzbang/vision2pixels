------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

with V2P.URL;
with V2P.Database;
with V2P.Context;
with V2P.Navigation_Links;
with V2P.Settings;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_Vote_Week_Photo;
with V2P.Template_Defs.Set_Global;

package body V2P.Callbacks.Web_Block is

   procedure User_Post_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set;
      Forum        : in     Database.Forum_Filter);

   ----------
   -- Exif --
   ----------

   procedure Exif
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Templates.Insert
           (Translations,
            Database.Get_Exif
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.TID)));
      end if;
   end Exif;

   ---------------------------
   -- Forum_Category_Filter --
   ---------------------------

   procedure Forum_Category_Filter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
   end Forum_Category_Filter;

   ------------------
   -- Forum_Filter --
   ------------------

   procedure Forum_Filter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FILTER_PAGE_SIZE,
            V2P.Context.Not_Null_Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE)));
   end Forum_Filter_Page_Size;

   ----------------
   -- Forum_List --
   ----------------

   procedure Forum_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations, Database.Get_Forums (Filter => Database.Forum_All));
   end Forum_List;

   -----------------------------
   -- Forum_Photo_List_Select --
   -----------------------------

   procedure Forum_Photo_List_Select
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Forums (Filter => Database.Forum_Photo));
   end Forum_Photo_List_Select;

   ----------------------------
   -- Forum_Text_List_Select --
   ----------------------------

   procedure Forum_Text_List_Select
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Forums (Filter => Database.Forum_Text));
   end Forum_Text_List_Select;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use V2P.Context;

      Admin     : constant Boolean :=
                    Context.Exist (Template_Defs.Set_Global.ADMIN)
                  and then Context.Get_Value
                    (Template_Defs.Set_Global.ADMIN) = "T";
      Page_Size : constant Navigation_Links.Page_Size :=
                    V2P.Context.Not_Null_Counter.Get_Value
                      (Context => Context.all,
                       Name    => Template_Defs.Set_Global.FILTER_PAGE_SIZE);
      Nav_From  : Positive :=
                    V2P.Context.Not_Null_Counter.Get_Value
                      (Context => Context.all,
                       Name    => Template_Defs.Set_Global.NAV_FROM);
      Nav_Links : Navigation_Links.Post_Ids.Vector;
      Nb_Lines  : Natural;
      Total     : Natural;
   begin
      --  ??? This function does exactly the same thing as
      --  V2P.Navigation_Links.Load_Pages
      --  This need some refactoring :
      --        - Split Get_Threads so that the procedure can returns
      --            * only the Post_Ids vector
      --             (as needed by V2P.Navigation_Links.Load_Pages)
      --            * and only the Template Set ?

      Database.Get_Threads
        (FID         => V2P.Context.Counter.Get_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.FID),
         From        => Nav_From,
         Admin       => Admin,
         Filter      => Database.Filter_Mode'Value (Context.Get_Value
           (Template_Defs.Set_Global.FILTER)),
         Filter_Cat  => Context.Get_Value
           (Template_Defs.Set_Global.FILTER_CATEGORY),
         Page_Size   => Page_Size,
         Order_Dir   => Database.Order_Direction'Value
           (Context.Get_Value (Template_Defs.Set_Global.ORDER_DIR)),
         Navigation  => Nav_Links,
         Set         => Translations,
         Nb_Lines    => Nb_Lines,
         Total_Lines => Total);

      Navigation_Links.Set (Context, Nav_Links);

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => Nav_From);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_RETURNED,
         Value   => Nb_Lines);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_TOTAL,
         Value   => Total);
   end Forum_Threads;

   -------------------
   -- Global_Rating --
   -------------------

   procedure Global_Rating
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Templates.Insert
           (Translations,
            Database.Get_Global_Rating
              (V2P.Context.Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.TID)));
      end if;
   end Global_Rating;

   ------------------
   -- Latest_Posts --
   ------------------

   procedure Latest_Posts
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Latest_Posts (Settings.Number_Latest_Posts));
   end Latest_Posts;

   ------------------
   -- Latest_Users --
   ------------------

   procedure Latest_Users
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
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
            Templates.Insert
              (Translations,
               Database.Get_Metadata
                 (V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID)));
         end if;
      end if;
   end Metadata;

   --------------
   -- New_Post --
   --------------

   procedure New_Post
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
               Database.Get_User_Rating_On_Post
                 (Uid => Context.Get_Value (Template_Defs.Set_Global.LOGIN),
                  Tid => V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID)));
         end if;
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert (Translations, Database.Get_Photo_Of_The_Week);
   end Photo_Of_The_Week;

   -----------------
   -- Quick_Login --
   -----------------

   procedure Quick_Login
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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

   -----------------------
   -- User_Comment_List --
   -----------------------

   procedure User_Comment_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User_Comment (Uid => User_Name, Textify => True));
   end User_Comment_List;

   -----------------------
   -- User_Message_List --
   -----------------------

   procedure User_Message_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set) is
   begin
      User_Post_List (Request, Context, Translations, Database.Forum_Text);
   end User_Message_List;

   ---------------
   -- User_Page --
   ---------------

   procedure User_Page
     (Request      : in Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);

   begin
      Templates.Insert
        (Translations, Database.Get_User_Page (Uid => User_Name));

      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Block_User_Page.USER_NAME, User_Name));
   end User_Page;

   ---------------------
   -- User_Photo_List --
   ---------------------

   procedure User_Photo_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set) is
   begin
      User_Post_List (Request, Context, Translations, Database.Forum_Photo);
   end User_Photo_List;

   --------------------
   -- User_Post_List --
   --------------------

   procedure User_Post_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set;
      Forum        : in     Database.Forum_Filter)
   is

      Admin      : constant Boolean :=
                     Context.Exist (Template_Defs.Set_Global.ADMIN)
                   and then Context.Get_Value
                     (Template_Defs.Set_Global.ADMIN) = "TRUE";
      URI        : constant String := Status.URI (Request);
      User_Name  : constant String := URL.User_Name (URI);
      Set        : Templates.Translate_Set;
      Navigation : Navigation_Links.Post_Ids.Vector;
      From       : Natural := 1;
      Nb_Lines   : Natural;
      Total      : Natural;
   begin
      Database.Get_Threads
        (User          => User_Name,
         Navigation    => Navigation,
         Set           => Set,
         Admin         => Admin,
         From          => From,
         Nb_Lines      => Nb_Lines,
         Total_Lines   => Total,
         Forum         => Forum,
         Only_Revealed => True);

      Templates.Insert (Translations, Set);
      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_RETURNED,
         Value   => Nb_Lines);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_NB_LINES_TOTAL,
         Value   => Total);
   end User_Post_List;

   ----------------------------
   -- User_Voted_Photos_List --
   ----------------------------

   procedure User_Voted_Photos_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI        : constant String := Status.URI (Request);
      User_Name  : constant String := URL.User_Name (URI);
      Set        : Templates.Translate_Set;
   begin
      Set := Database.Get_User_Voted_Photos (User_Name);
      Templates.Insert (Translations, Set);
   end User_Voted_Photos_List;

   ---------------------
   -- Vote_Week_Photo --
   ---------------------

   procedure Vote_Week_Photo
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
               Database.Has_User_Vote
                 (Uid => Context.Get_Value (Template_Defs.Set_Global.LOGIN),
                  Tid => V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.TID))));
      end if;
   end Vote_Week_Photo;

end V2P.Callbacks.Web_Block;
