------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
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

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Log;
with AWS.Services.Dispatchers.URI;
with AWS.Services.ECWF.Registry;
with AWS.Services.ECWF.Context;
with AWS.Session;
with AWS.Status;
with AWS.Templates;

with V2P.Database;
with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Forum_Post;
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Error;
with V2P.Template_Defs.Global;
with V2P.Template_Defs.Iframe_Photo_Post;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Post;
with V2P.Template_Defs.Block_New_Photo;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_Forum_Navigate;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_Forum_List;
with V2P.Template_Defs.R_Block_Forum_Filter;
with V2P.Template_Defs.R_Block_Comment_Form_Enter;
with V2P.Template_Defs.R_Block_Post_Form_Enter;
with V2P.Template_Defs.R_Block_Metadata_Form_Enter;
with V2P.Wiki;

with Image.Data;
with Image.Metadata;
with Settings;

package body V2P.Web_Server is

   use AWS;

   HTTP            : Server.HTTP;
   Configuration   : Config.Object;
   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   -------------------------
   --  Standard Callbacks --
   -------------------------

   function Default_Xml_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback for xml action

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   function CSS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element CSS callback

   function Photos_Callback (Request : in Status.Data) return Response.Data;
   --  Photos callback

   function Thumbs_Callback (Request : in Status.Data) return Response.Data;
   --  Thumbs callback

   --------------------
   -- ECWF Callbacks --
   --------------------

   procedure Forum_Entry_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum entry callback

   procedure Forum_Threads_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum threads callback

   procedure Login_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Login callback

   procedure Logout_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Logout callback

   procedure Onchange_Forum_List_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when a new forum is selected

   procedure Onchange_Filter_Forum
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when changing the forum sorting

   procedure Onsubmit_Comment_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting a new comment

   procedure Onsubmit_Metadata_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting new metadata

   procedure Onsubmit_Post_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting a new post

   procedure New_Photo_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Adds a new photo in user tmp photo table


   procedure Main_Page_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Display v2p main page

   procedure Context_Filter (Context : access Services.ECWF.Context.Object);
   --  Update the context filter

   --------------------------
   -- Other local routines --
   --------------------------

   function Is_Valid_Comment (Comment : in String) return Boolean;
   --  Check if the comment is valid

   --------------------
   -- Context_Filter --
   --------------------

   procedure Context_Filter (Context : access Services.ECWF.Context.Object) is
   begin
      if not Context.Exist (Template_Defs.Global.FILTER) then
         Context.Set_Value
           (Template_Defs.Global.FILTER,
            Database.Filter_Mode'Image (Database.All_Messages));

         if Settings.Descending_Order then
            Context.Set_Value
              (Template_Defs.Global.ORDER_DIR,
               Database.Order_Direction'Image (Database.DESC));
         else
            Context.Set_Value
              (Template_Defs.Global.ORDER_DIR,
               Database.Order_Direction'Image (Database.ASC));
         end if;
      end if;
   end Context_Filter;

   ------------------
   -- CSS_Callback --
   ------------------

   function CSS_Callback (Request : in Status.Data) return Response.Data is
      SID          : constant Session.Id := Status.Session (Request);
      URI          : constant String := Status.URI (Request);
      File         : constant String := URI (URI'First + 1 .. URI'Last);
      Translations : Templates.Translate_Set;
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc ("LOGIN", String'(Session.Get (SID, "LOGIN"))));
      return Response.Build
        (MIME.Content_Type (File),
         String'(Templates.Parse (File, Translations)));
   end CSS_Callback;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback
     (Request : in Status.Data) return Response.Data
   is
      use type Messages.Status_Code;
      URI          : constant String := Status.URI (Request);
      SID          : constant Session.Id := Status.Session (Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin
      if Session.Exist (SID, "LOGIN") then
         Templates.Insert
           (Translations,
            Templates.Assoc ("LOGIN", String'(Session.Get (SID, "LOGIN"))));
      end if;

      --  Adds some URL

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.FORUM_THREAD_URL,
            Template_Defs.Forum_Threads.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.FORUM_POST_URL,
            Template_Defs.Forum_Post.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.FORUM_ENTRY_URL,
            Template_Defs.Forum_Entry.URL));

      --  Insert the thumb path

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Global.THUMB_SOURCE_PREFIX, Thumbs_Source_Prefix));

      Web_Page := Services.ECWF.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         return Services.ECWF.Registry.Build
           (Template_Defs.Error.URL, Request, Translations);

      else
         return Web_Page;
      end if;
   end Default_Callback;

   --------------------------
   -- Default_Xml_Callback --
   --------------------------

   function Default_Xml_Callback
     (Request : in Status.Data) return Response.Data
   is
      URI  : constant String := Status.URI (Request);
      File : constant String := "xml" & '/' & URI (URI'First + 5 .. URI'Last);
   begin
      return Response.File (MIME.Text_XML, File);
   end Default_Xml_Callback;

      --------------------------
   -- Forum_Entry_Callback --
   --------------------------

   procedure Forum_Entry_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID         : constant Session.Id := Status.Session (Request);
      P           : constant Parameters.List := Status.Parameters (Request);
      TID         : constant String :=
                      Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.TID);
      Login       : constant String := Session.Get (SID, "LOGIN");
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the session
      Context.Set_Value ("TID", TID);

      if TID = "" then
         --  Page does not exit
         return;
      end if;

      Context_Filter (Context);

      if not Settings.Anonymous_Visit_Counter then
         --  Do not count anonymous click
         if Login = "" then
            Count_Visit := False;

         else
            if Settings.Ignore_Author_Click
              and then Database.Is_Author (Login, TID)
            then
               --  Do not count author click
               Count_Visit := False;
            end if;
         end if;
      end if;

      if Count_Visit then
         Database.Increment_Visit_Counter (TID);
      end if;

      --  Insert navigation links (previous and next post)
      --  ??? We should use the context here

      Templates.Insert
        (Translations, Database.Get_Thread_Navigation_Links
           (Fid => Context.Get_Value ("FID"),
            Tid => TID,
            Filter => Database.Filter_Mode'Value
              (Context.Get_Value (Template_Defs.Global.FILTER)),
            Order_Dir => Database.Order_Direction'Value
              (Context.Get_Value (Template_Defs.Global.ORDER_DIR))));

      Templates.Insert (Translations, Database.Get_Entry (TID));
   end Forum_Entry_Callback;

   ----------------------------
   -- Forum_Threads_Callback --
   ----------------------------

   procedure Forum_Threads_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      P    : constant Parameters.List := Status.Parameters (Request);
      FID  : constant String :=
               Parameters.Get (P, Template_Defs.Forum_Threads.HTTP.FID);
      From : Positive := 1;
   begin
      --  Set forum Id into the context
      Context.Set_Value ("FID", FID);

      if Context.Exist ("TID") then
         Context.Remove ("TID");
      end if;

      if Parameters.Exist
        (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM)
      then
         From := Positive'Value
           (Parameters.Get (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM));
      end if;

      Context_Filter (Context);

      Templates.Insert
        (Translations,
         Database.Get_Threads
           (FID, From => From,
            Order_Dir => Database.Order_Direction'Value
              (Context.Get_Value (Template_Defs.Global.ORDER_DIR))));
   end Forum_Threads_Callback;

   ----------------------
   -- Is_Valid_Comment --
   ----------------------

   function Is_Valid_Comment (Comment : in String) return Boolean is
   begin
      if Comment = "" then
         --  Does not accept empty comment
         return False;
      end if;

      --  ??? Checks if the same comment is already in user context

      return True;
   end Is_Valid_Comment;

   --------------------
   -- Login_Callback --
   --------------------

   procedure Login_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID      : constant Session.Id := Status.Session (Request);
      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String := Parameters.Get (P, "LOGIN");
      Password : constant String := Database.Get_Password (Login);
   begin
      if Password = Parameters.Get (P, "PASSWORD") then
         Session.Set (SID, "LOGIN", Login);
         Session.Set (SID, "PASSWORD", Password);

         --  Set user's filtering preference
         --  ??? to be done when user's preferences are implemented

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Login.LOGIN,
               String'(Session.Get (SID, "LOGIN"))));
      end if;
   end Login_Callback;

   ---------------------
   -- Logout_Callback --
   ---------------------

   procedure Logout_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Session.Delete (SID);

      --  Override LOGIN previously set in Default_Callback ?

      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.R_Block_Logout.LOGIN_FORM,
           String'(Templates.Parse
             (Template_Defs.Block_Login.Template))));
   end Logout_Callback;

   ------------------------
   -- Main_Page_Callback --
   ------------------------

   procedure Main_Page_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist ("TID") then
         Context.Remove ("TID");
      end if;
      if Context.Exist ("FID") then
         Context.Remove ("FID");
      end if;

      Context_Filter (Context);
   end Main_Page_Callback;

   ------------------------
   -- New_Photo_Callback --
   ------------------------

   procedure New_Photo_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
   pragma Unreferenced (Context);
      use Image.Data;

      SID          : constant Session.Id := Status.Session (Request);
      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String := Session.Get (SID, "LOGIN");
      Filename     : constant String := Parameters.Get (P, "FILENAME");

      Images_Path  : String renames Settings.Get_Images_Path;

      New_Image    : Image_Data;

   begin
      Init (Img => New_Image, Filename => Filename);

      if New_Image.Init_Status /= Image_Created then
         Templates.Insert
           (Translations,
            Templates.Assoc (Template_Defs.Main_Page.V2P_ERROR,
              Image_Init_Status'Image (New_Image.Init_Status)));

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Main_Page.EXCEED_MAXIMUM_IMAGE_DIMENSION,
               Image_Init_Status'Image
                 (Image.Data.Exceed_Max_Image_Dimension)));

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Main_Page.EXCEED_MAXIMUM_SIZE,
               Image_Init_Status'Image
                 (Image.Data.Exceed_Max_Size)));
      else
         declare
            New_Photo_Filename : constant String
              := New_Image.Filename
                ((Images_Path'Length + 2) .. New_Image.Filename'Last);
            Pid                : constant String
              := Database.Insert_Photo
                (Login,
                 New_Photo_Filename,
                 Natural (New_Image.Dimension.Width),
                 Natural (New_Image.Dimension.Height),
                 Natural (New_Image.Dimension.Size));
         begin
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.Iframe_Photo_Post.NEW_PHOTO_ID,
                  Pid));
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.Iframe_Photo_Post.NEW_PHOTO_FILENAME,
                  New_Photo_Filename));
         end;
      end if;
   end New_Photo_Callback;

   ---------------------------
   -- Onchange_Filter_Forum --
   ---------------------------

   procedure Onchange_Filter_Forum
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Translations);
      P      : constant Parameters.List := Status.Parameters (Request);
      Filter : constant String := Parameters.Get (P, "sel_filter_forum");
   begin
      --  Keep the sorting scheme into the session
      --  ?? we need to add this into the user's preferences
      Context.Set_Value ("FILTER", Filter);
   end Onchange_Filter_Forum;

   ----------------------------------
   -- Onchange_Forum_List_Callback --
   ----------------------------------

   procedure Onchange_Forum_List_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P   : constant Parameters.List := Status.Parameters (Request);
      Fid : constant String := Parameters.Get (P, "sel_forum_list");
      --  ??
   begin
      Templates.Insert (Translations, Database.Get_Categories (Fid));
   end Onchange_Forum_List_Callback;

   ------------------------------------------
   -- Onsubmit_Comment_Form_Enter_Callback --
   ------------------------------------------

   procedure Onsubmit_Comment_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use Template_Defs;
      SID          : constant Session.Id := Status.Session (Request);
      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String := Session.Get (SID, "LOGIN");
      Anonymous    : constant String :=
                       Parameters.Get
                         (P, Block_New_Comment.HTTP.ANONYMOUS_USER);
      Name         : constant String :=
                       Parameters.Get (P, Block_New_Comment.HTTP.NAME);
      Comment      : constant String :=
                       Parameters.Get (P, Block_New_Comment.HTTP.COMMENT);
      Parent_Id    : constant String := Parameters.Get (P, "PARENT_ID");
      --  ??? no ref in template
      Pid          : constant String := Parameters.Get (P, "PID");
      --  ??? no ref in template
      Tid          : constant String :=
                       Parameters.Get (P, Block_New_Comment.HTTP.TID);
      Comment_Wiki : constant String := V2P.Wiki.Wiki_To_HTML (Comment);
      Last_Comment : constant String := Context.Get_Value ("LAST_COMMENT");
   begin
      if Login = "" and then Anonymous = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR, "ERROR_NO_LOGIN"));

      elsif Last_Comment = Comment then
         --   This is a duplicated post

         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR_DUPLICATED, "ERROR"));

      elsif Tid /= "" and not Is_Valid_Comment (Comment_Wiki) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (R_Block_Comment_Form_Enter.ERROR, "ERROR"));
            --  ??? Adds an error message
      else
         declare
            Cid : constant String := Database.Insert_Comment
              (Login, Anonymous, Tid, Name, Comment_Wiki, Pid);
         begin
            --  Adds the new comment in context to prevent duplicated post

            Context.Set_Value ("LAST_COMMENT", Comment);

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
         end;
      end if;
   end Onsubmit_Comment_Form_Enter_Callback;

   -------------------------------------------
   -- Onsubmit_Metadata_Form_Enter_Callback --
   -------------------------------------------

   procedure Onsubmit_Metadata_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use Image.Metadata;

      function Get (Parameter_Name : in String) return Geo_Coordinate;
      --  Returns the given parameter or zero if not found

      P : constant Parameters.List := Status.Parameters (Request);

      ---------
      -- Get --
      ---------

      function Get (Parameter_Name : in String) return Geo_Coordinate is
         Param : constant String := Parameters.Get (P, Parameter_Name);
      begin
         if Param = "" then
            return 0.0;
         else
            return Geo_Coordinate'Value (Param);
         end if;
      end Get;

      Latitude_Coord      : constant Geo_Coordinate := Get
        (Template_Defs.Block_Metadata.HTTP.latitude);
      Longitude_Coord     : constant Geo_Coordinate := Get
        (Template_Defs.Block_Metadata.HTTP.longitude);
      Latitude_Position   : Latitude;
      Longitude_Postition : Longitude;

   begin
      if Latitude_Coord = 0.0 or else Longitude_Coord = 0.0
        or else not Context.Exist ("TID")
      then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Metadata_Form_Enter.ERROR, "ERROR"));
         --  ??? Adds an error message
         return;
      end if;

      Latitude_Position.Format (Latitude_Coord);
      Longitude_Postition.Format (Longitude_Coord);

      Database.Insert_Metadata
        (Context.Get_Value ("TID"),
         Float (Latitude_Coord),
         Float (Longitude_Coord),
         Image.Metadata.Image (Latitude_Position),
         Image.Metadata.Image (Longitude_Postition));
   end Onsubmit_Metadata_Form_Enter_Callback;

   ---------------------------------------
   -- Onsubmit_Post_Form_Enter_Callback --
   ---------------------------------------

   procedure Onsubmit_Post_Form_Enter_Callback
     (Request      : in     Status.Data;
      Context      : access Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID          : constant Session.Id := Status.Session (Request);
      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String := Session.Get (SID, "LOGIN");
      Name         : constant String := Parameters.Get (P, "NAME");
      Comment      : constant String := Parameters.Get (P, "COMMENT");
      Pid          : constant String := Parameters.Get (P, "PID");
      CID          : constant String := Parameters.Get (P, "CATEGORY");
      Forum        : constant String := Parameters.Get (P, "FORUM");
      Last_Name    : constant String := Context.Get_Value ("LAST_POST_NAME");
      Comment_Wiki : constant String := V2P.Wiki.Wiki_To_HTML (Comment);
   begin
      if Login = "" and then CID = "" then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR,
               "POST SUBMIT ERROR"));
         --  ??? Adds an error message
         return;

      elsif Last_Name = Name then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.R_Block_Post_Form_Enter.ERROR_DUPLICATED,
               "ERROR_DUPLICATE_POST"));

      else
         declare
            Post_Id : constant String :=
              Database.Insert_Post (Login, CID, Name, Comment_Wiki, Pid);
         begin
            if Post_Id /= "" then

               --  Set new context TID (needed by
               --  Onsubmit_Metadata_Form_Enter_Callback)

               Context.Set_Value ("TID", Post_Id);
               Context.Set_Value ("LAST_POST_NAME", Name);

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.R_Block_Post_Form_Enter.URL,
                     Template_Defs.Forum_Entry.URL & "?FID=" & Forum
                     & "&amp;TID=" & Post_Id));
            else
               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.R_Block_Post_Form_Enter.ERROR,
                     "DATABASE INSERT FAILED"));
            end if;
         end;
      end if;
      if Pid /= "" then
         Onsubmit_Metadata_Form_Enter_Callback (Request, Context,
                                                Translations);
      end if;
   end Onsubmit_Post_Form_Enter_Callback;

   ---------------------
   -- Photos_Callback --
   ---------------------

   function Photos_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Settings.Get_Images_Path & "/"
                 & URI (URI'First +
                          Images_Source_Prefix'Length + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (File), File);
   end Photos_Callback;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/xml_",
         Action => Dispatchers.Callback.Create (Default_Xml_Callback'Access),
         Prefix => True);
      --  All URLs starting with /xml_ are handled by a specific callback
      --  returning the corresponding file in the xml directory.

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/we_js",
         Action => Dispatchers.Callback.Create (WEJS_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/css",
         Action => Dispatchers.Callback.Create (CSS_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Images_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Thumbs_Source_Prefix,
         Action => Dispatchers.Callback.Create (Thumbs_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register_Default_Callback
        (Main_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));
      --  This default callback will handle all ECWF callbacks

      --  Register ECWF pages

      Services.ECWF.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_login_form_enter,
         Template_Defs.R_Block_Login.Template,
         Login_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_logout_enter,
         Template_Defs.R_Block_Logout.Template,
         Logout_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Forum_Threads.Ajax.onchange_sel_filter_forum,
         Template_Defs.R_Block_Forum_Filter.Template,
         Onchange_Filter_Forum'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onchange_sel_forum_list,
         Template_Defs.R_Block_Forum_List.Template,
         Onchange_Forum_List_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onsubmit_comment_form,
         Template_Defs.R_Block_Comment_Form_Enter.Template,
         Onsubmit_Comment_Form_Enter_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_New_Post.Ajax.onsubmit_post_form,
         Template_Defs.R_Block_Post_Form_Enter.Template,
         Onsubmit_Post_Form_Enter_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_Metadata.Ajax.onsubmit_metadata_post,
         Template_Defs.R_Block_Metadata_Form_Enter.Template,
         Onsubmit_Metadata_Form_Enter_Callback'Access,
         MIME.Text_XML);

      Services.ECWF.Registry.Register
        (Template_Defs.Forum_Entry.URL,
         Template_Defs.Forum_Entry.Template,
         Forum_Entry_Callback'Access);

      Services.ECWF.Registry.Register
        (Template_Defs.Block_New_Photo.URL,
         Template_Defs.Iframe_Photo_Post.Template,
         New_Photo_Callback'Access);

      Services.ECWF.Registry.Register
        (Template_Defs.Forum_Threads.URL,
         Template_Defs.Forum_Threads.Template,
         Forum_Threads_Callback'Access);

      Services.ECWF.Registry.Register
        (Template_Defs.Main_Page.URL,
         Template_Defs.Main_Page.Template,
         Main_Page_Callback'Access);

      Services.ECWF.Registry.Register
        (Template_Defs.Error.URL,
         Template_Defs.Error.Template,
         null);

      Services.ECWF.Registry.Register
        (Template_Defs.Forum_Post.URL,
         Template_Defs.Forum_Post.Template,
         null);

      --  Log control

      Server.Log.Start (HTTP, Auto_Flush => True);
      Server.Log.Start_Error (HTTP);

      --  Server configuration

      Config.Set.Session (Configuration, True);
      Config.Set.Upload_Directory (Configuration, "./uploads/");
      Config.Set.Admin_URI (Configuration, "/admin");

      --  Starting server

      Server.Start (HTTP, Main_Dispatcher, Configuration);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Server.Shutdown (HTTP);
   end Stop;

   ---------------------
   -- Thumbs_Callback --
   ---------------------

   function Thumbs_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Settings.Get_Thumbs_Path & "/"
                 & URI (URI'First +
                          Thumbs_Source_Prefix'Length + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (File), File);
   end Thumbs_Callback;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Server.Wait (Server.Forever);
   end Wait;

   -------------------
   -- WEJS_Callback --
   -------------------

   function WEJS_Callback (Request : in Status.Data) return Response.Data is
      URI          : constant String := Status.URI (Request);
      File         : constant String := URI (URI'First + 1 .. URI'Last);
      Translations : Templates.Translate_Set;
   begin
      return Response.Build
        (MIME.Content_Type (File),
         String'(Templates.Parse (File, Translations)));
   end WEJS_Callback;

end V2P.Web_Server;
