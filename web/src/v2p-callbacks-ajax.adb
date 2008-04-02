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

with Ada.Strings.Unbounded;

with GNAT.String_Split;

with AWS.Parameters;
with AWS.Session;
with AWS.SMTP.Client;

with Image.Metadata.Geographic;

with Morzhol.Logs;

with V2P.Context;
with V2P.Database.Search;
with V2P.User_Validation;
with V2P.Wiki;
with V2P.Syndication;

with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_New_Text_Entry;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Page_Search;
with V2P.Template_Defs.Page_User_Register;
with V2P.Template_Defs.Set_Global;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_Forum_Category_Filter;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_Forum_Filter_Page_Size;
with V2P.Template_Defs.Block_Forum_Sort;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Chunk_Forum_List_Select;
with V2P.Template_Defs.Chunk_Search_User;
with V2P.Template_Defs.Email_User_Validation;
with V2P.Template_Defs.R_Block_Forum_Filter;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Post_Form_Enter;
with V2P.Template_Defs.R_Block_Comment_Form_Enter;
with V2P.Template_Defs.R_Block_User_Page_Edit_Form_Enter;
with V2P.Template_Defs.R_Page_Search;
with V2P.Template_Defs.R_Page_User_Register;

package body V2P.Callbacks.Ajax is

   use Ada.Strings.Unbounded;
   use GNAT;

   Module : constant Morzhol.Logs.Module_Name := "Ajax";

   -----------
   -- Login --
   -----------

   procedure Login
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
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
         Session.Set (SID, Template_Defs.Set_Global.ADMIN, User_Data.Admin);

         --  Set last logged status

         Database.Set_Last_Logged (Login);

         --  Set user's filtering preference
         --  ??? to be done when user's preferences are implemented

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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID : constant Session.Id := Status.Session (Request);
   begin
      Session.Delete (SID);

      Context.Remove (Template_Defs.Set_Global.LOGIN);
      Context.Remove (Template_Defs.Set_Global.ADMIN);

      --  Remove the login information from the translate table

      Templates.Remove (Translations, Template_Defs.Set_Global.LOGIN);
   end Logout;

   ------------------------------------
   -- Onchange_Category_Filter_Forum --
   ------------------------------------

   procedure Onchange_Category_Filter_Forum
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Category_Filter.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bfcf_forum_category_filter_set);
   begin
      --  Keep the sorting scheme into the session
      --  ?? we need to add this into the user's preferences
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
   -- Onchange_Filter_Forum --
   ---------------------------

   procedure Onchange_Filter_Forum
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Filter.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String :=
                 Parameters.Get (P, HTTP.bff_forum_filter_set);
   begin
      --  Keep the sorting scheme into the session
      --  ?? we need to add this into the user's preferences
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Filter_Page_Size.HTTP;

      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String          :=
                 Parameters.Get (P, HTTP.bffps_forum_filter_pagesize);
   begin
      --  Keep the sorting scheme into the session
      --  ?? we need to add this into the user's preferences

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

   -------------------------
   -- Onchange_Forum_List --
   -------------------------

   procedure Onchange_Forum_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      package HTTP renames Template_Defs.Block_Forum_Sort.HTTP;
      P    : constant Parameters.List := Status.Parameters (Request);
      Sort : constant String := Parameters.Get (P, HTTP.bfs_forum_sort_set);
   begin
      --  Keep the sorting scheme into the session
      --  ?? we need to add this into the user's preferences
      Context.Set_Value (Template_Defs.Set_Global.FORUM_SORT, Sort);

      --  Set the context filter to seven days if the sorting is set to
      --  NEED_ATTENTION.

      if Sort = Template_Defs.Block_Forum_Sort.Set.NEED_ATTENTION then
         --  Store current filter

         Context.Set_Value
           (Template_Defs.Set_Global.PREVIOUS_FILTER,
            Context.Get_Value (Template_Defs.Set_Global.FILTER));

         Context.Set_Value
           (Template_Defs.Set_Global.FILTER,
            Template_Defs.Block_Forum_Filter.Set.SEVEN_DAYS);

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Forum_Filter.FORCE_FILTER,
               Template_Defs.Block_Forum_Filter.Set.SEVEN_DAYS));

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

   ----------------------------
   -- Onclick_Goto_Next_Page --
   ----------------------------

   procedure Onclick_Goto_Next_Page
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use Template_Defs;
      Last_Nb_Viewed : constant Natural :=
                         V2P.Context.Counter.Get_Value
                           (Context => Context.all,
                            Name    => Set_Global.NAV_NB_LINES_RETURNED);
      Total          : constant Natural :=
                         V2P.Context.Counter.Get_Value
                           (Context => Context.all,
                            Name    => Set_Global.NAV_NB_LINES_TOTAL);
      From           : Positive         :=
                         V2P.Context.Not_Null_Counter.Get_Value
                           (Context => Context.all,
                            Name    => Set_Global.NAV_FROM);
   begin
      if From + Last_Nb_Viewed < Total then
         From := From + Last_Nb_Viewed;
      end if;

      --  Update FROM counter

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => From);

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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      use Template_Defs;
      Last_From      : constant Positive :=
                         V2P.Context.Not_Null_Counter.Get_Value
                           (Context => Context.all,
                            Name    => Set_Global.NAV_FROM);
      Last_Nb_Viewed : constant Integer :=
                         V2P.Context.Counter.Get_Value
                           (Context => Context.all,
                            Name    => Set_Global.NAV_NB_LINES_RETURNED);
      From           : Positive := 1;
   begin

      if Last_From > Last_Nb_Viewed then
         From := Last_From - Last_Nb_Viewed;
      end if;

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => From);

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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      TID : constant Database.Id := V2P.Context.Counter.Get_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.TID);
   begin
      Templates.Insert (Translations, Database.Toggle_Hidden_Status (TID));
   end Onclick_Hidden_Status_Toggle;

   -----------------------------
   -- Onclick_Vote_Week_Photo --
   -----------------------------

   procedure Onclick_Vote_Week_Photo
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
      Login        : constant String :=
                       Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      TID          : constant Database.Id :=
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
      Get_Photo_ID : constant String :=
                       Parameters.Get
                         (P, Block_New_Comment.HTTP.bnc_comment_pid);
      Parent_Id    : constant String :=
                       Parameters.Get (P, Page_Forum_Entry.HTTP.pfe_PARENT_ID);
      Comment_Wiki : constant String := V2P.Wiki.Wiki_To_HTML (Comment);
      Last_Comment : constant String :=
                       Context.Get_Value (Set_Global.CONTEXT_LAST_COMMENT);
      TID          : constant Database.Id :=
                       V2P.Context.Counter.Get_Value
                         (Context => Context.all,
                          Name    => Template_Defs.Set_Global.TID);

      Forum_Type   : V2P.Database.Forum_Type := V2P.Database.Forum_Text;
      Photo_Id     : V2P.Database.Id;

      function Is_Valid_Comment (Comment : in String) return Boolean;
      --  Check if the comment is valid

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
        and then not Is_Valid_Comment (Comment_Wiki)
      then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR, "ERROR"));
         --  ??? Adds an error message

      else
         Insert_Comment : declare
            Cid : constant Database.Id := Database.Insert_Comment
              (Uid       => Login,
               Anonymous => Anonymous,
               Thread    => TID,
               Name      => "",
               Comment   => Comment_Wiki,
               Pid       => Photo_Id);
         begin
            --  Adds the new comment in context to prevent duplicated post

            Context.Set_Value
              (Set_Global.CONTEXT_LAST_COMMENT,
               Comment & '@' & Database.To_String (TID));

            Templates.Insert (Translations, Database.Get_Comment (Cid));
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (R_Block_Comment_Form_Enter.PARENT_ID, Parent_Id));
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (R_Block_Comment_Form_Enter.COMMENT_LEVEL, "1"));
            --  Does not support threaded view for now

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
                  Forum_Type => Forum_Type));
         end Insert_Comment;
      end if;
   end Onsubmit_Comment_Form_Enter;

   ----------------------------------
   -- Onsubmit_Metadata_Form_Enter --
   ----------------------------------

   procedure Onsubmit_Metadata_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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

   ------------------------------
   -- Onsubmit_Post_Form_Enter --
   ------------------------------

   procedure Onsubmit_Post_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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
      CID          : constant Database.Id :=
                       Database.Id'Value
                         (Parameters.Get
                            (P, Chunk_Forum_List_Select.HTTP.CATEGORY));
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

      if Login = ""
        or else CID = Database.Empty_Id
        or else Name = ""
      then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               "POST SUBMIT ERROR"));
         --  ??? Adds an error message

      else
         if Last_Name = Name then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.R_Block_Post_Form_Enter.ERROR_DUPLICATED,
                  "ERROR_DUPLICATE_POST"));

         else

            Insert_Post : declare
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

                  if PID /= Database.Empty_Id then
                     --  Regenerate RSS feed

                     Syndication.Update_RSS_Last_Photos;
                  end if;

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
      end if;
   end Onsubmit_Post_Form_Enter;

   --------------------------------
   -- Onsubmit_Pur_Register_User --
   --------------------------------

   procedure Onsubmit_Pur_Register_User
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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

      Send_Mail : declare
         Localhost : constant SMTP.Receiver :=
                       SMTP.Client.Initialize ("localhost");
         Key       : constant String :=
                       User_Validation.Key (Login, Password, Email);
         Result    : SMTP.Status;
         Set       : Templates.Translate_Set;
      begin
         Templates.Insert (Set, Templates.Assoc ("USER_LOGIN", Login));
         Templates.Insert (Set, Templates.Assoc ("USER_EMAIL", Email));
         Templates.Insert (Set, Templates.Assoc ("KEY", Key));

         SMTP.Client.Send
           (Server  => Localhost,
            From    => SMTP.E_Mail ("V2P", "no-reply"),
            To      => SMTP.E_Mail (Email, Email),
            Subject => "Enregistrement sur Vision2Pixels",
            Message => Templates.Parse
              (Template_Defs.Email_User_Validation.Template, Set),
            Status  => Result);

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
      end Send_Mail;
   end Onsubmit_Pur_Register_User;

   -------------------
   -- Onsubmit_Rate --
   -------------------

   procedure Onsubmit_Rate
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use Template_Defs;

      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String :=
                   Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      TID      : constant Database.Id :=
                   V2P.Context.Counter.Get_Value
                     (Context => Context.all,
                      Name    => Template_Defs.Set_Global.TID);
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
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      P                   : constant Parameters.List :=
                              Status.Parameters (Request);
      Login               : constant String :=
                              Context.Get_Value
                                (Template_Defs.Set_Global.LOGIN);
      User, Post, Comment : Boolean := False;
      S_Split             : String_Split.Slice_Set;
      pragma Unreferenced (Post, Comment);
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
            W_Set    : Database.Search.Word_Set
              (1 .. Natural (String_Split.Slice_Count (S_Split)));
            Last     : Natural := 0;
            Result_U : Unbounded_String;
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
                  Append
                    (Result_U,
                     Unbounded_String'(Templates.Parse
                       (Template_Defs.Chunk_Search_User.Template,
                          Database.Search.Users (W_Set (1 .. Last)))));
               end if;
            end if;

            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.R_Page_Search.SEARCH_RESULTS_USERS, Result_U));
         end;
      end if;
   end Onsubmit_Search_Form;

   ----------------------------------------
   -- Onsubmit_User_Page_Edit_Form_Enter --
   ----------------------------------------

   procedure Onsubmit_User_Page_Edit_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
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

end V2P.Callbacks.Ajax;
