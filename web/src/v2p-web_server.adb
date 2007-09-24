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

with Ada.Text_IO;

with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Registry;
with AWS.Services.Web_Block.Context;
with AWS.Session;
with AWS.Status;
with AWS.Templates;

with Gwiad.Web.Virtual_Host;
with Gwiad.Plugins.Websites.Registry;
with Morzhol.OS;

with V2P.Database;
with V2P.Context;
with V2P.Web_Ajax_Callbacks;

with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Forum_Post;
with V2P.Template_Defs.Admin;
with V2P.Template_Defs.User_Page;
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Error;
with V2P.Template_Defs.Global;
with V2P.Template_Defs.Iframe_Photo_Post;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_Forum_Navigate;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Post;
with V2P.Template_Defs.Block_New_Photo;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_Hidden_Status;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Forum_List;
with V2P.Template_Defs.R_Block_Forum_Filter;
with V2P.Template_Defs.R_Block_Comment_Form_Enter;
with V2P.Template_Defs.R_Block_Post_Form_Enter;
with V2P.Template_Defs.R_Block_Metadata_Form_Enter;
with V2P.Template_Defs.R_Block_User_Page_Edit_Form_Enter;

with Image.Data;
with Settings;

with Gwiad.Plugins.Websites;

package body V2P.Web_Server is

   use Ada;
   use AWS;

   use Morzhol.OS;

   use AWS.Services.Web_Block.Registry;
   use Gwiad.Plugins.Websites;

   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   XML_Path         : constant String :=
                        Directories.Compose
                          (Containing_Directory => Gwiad_Plugin_Path,
                           Name                 => "xml");
   XML_Prefix_URI   : constant String := "/xml_";
   CSS_URI          : constant String := "/css";
   Web_JS_URI       : constant String := "/we_js";

   V2p_Lib_Path     : constant String :=
                        Gwiad.Plugins.Get_Last_Library_Path;

   -------------------------
   --  Standard Callbacks --
   -------------------------

   function Default_XML_Callback
     (Request : in Status.Data) return String;
   --  Default callback for xml action

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function Website_Data (Request : in Status.Data) return Response.Data;
   --  Website data (images, ...) callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   function CSS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element CSS callback

   function Photos_Callback (Request : in Status.Data) return Response.Data;
   --  Photos callback

   function Thumbs_Callback (Request : in Status.Data) return Response.Data;
   --  Thumbs callback

   --------------------
   -- Page callbacks --
   --------------------

   procedure Main_Page_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Display v2p main page

   procedure New_Photo_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Adds a new photo in user tmp photo table

   procedure Forum_Entry_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum entry callback

   procedure Forum_Threads_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum threads callback

   -------------
   --  Gwiad  --
   -------------

   procedure Unregister (Name : in Website_Name);
   --  Unregister website

   --------------------------
   -- Other local routines --
   --------------------------

   function Get_Images_Path return String;
   --  Returns the current image path for the running plugin

   ------------------
   -- CSS_Callback --
   ------------------

   function CSS_Callback (Request : in Status.Data) return Response.Data is
      SID          : constant Session.Id := Status.Session (Request);
      URI          : constant String := Status.URI (Request);
      File         : constant String :=
                      Gwiad_Plugin_Path & Directory_Separator
                         & URI (URI'First + 1 .. URI'Last);
      Translations : Templates.Translate_Set;
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Global.LOGIN,
           String'(Session.Get (SID, Template_Defs.Global.LOGIN))));
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
      if Session.Exist (SID, Template_Defs.Global.LOGIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Global.LOGIN,
               String'(Session.Get (SID, Template_Defs.Global.LOGIN))));
      end if;

      if Session.Exist (SID, Template_Defs.Global.ADMIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Global.ADMIN,
               String'(Session.Get (SID, Template_Defs.Global.ADMIN))));
      end if;

      --  Adds Version number

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.V2P_VERSION,
            V2P.Version));

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

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.ADMIN_URL,
            Template_Defs.Admin.URL));

      --  Insert the thumb path

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Global.THUMB_SOURCE_PREFIX,
            Settings.Thumbs_Source_Prefix));

      Web_Page := Services.Web_Block.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         Web_Page := Services.Web_Block.Registry.Build
           (Template_Defs.Error.URL, Request, Translations);
      end if;

      return Web_Page;

   exception
      when others =>
         Fatal_Error :
         declare
         begin
            Ada.Text_IO.Put_Line ("fatal error");

            --  ??? Here we need to know the MIME type
            --  Without knowing it returns XML to avoid client browser warning
            return Response.Build
              (Message_Body => "<p>Internal error</p>",
               Content_Type => MIME.Text_XML);
         end Fatal_Error;
   end Default_Callback;

   --------------------------
   -- Default_XML_Callback --
   --------------------------

   function Default_XML_Callback (Request : in Status.Data) return String is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               XML_Path & '/' &  URI (URI'First + 5 .. URI'Last);
   begin
      return File;
   end Default_XML_Callback;
   --------------------------
   -- Forum_Entry_Callback --
   --------------------------

   procedure Forum_Entry_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID         : constant Session.Id := Status.Session (Request);
      P           : constant Parameters.List := Status.Parameters (Request);
      TID         : constant String :=
                      Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.TID);
      Login       : constant String :=
                      Session.Get (SID, Template_Defs.Global.LOGIN);
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the context

      Context.Set_Value (Template_Defs.Global.TID, TID);

      if TID /= "" then
         V2P.Context.Context_Filter (Context);

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

         Insert_Links : declare
            Selected_Post : constant V2P.Context.Post_Ids.Vector :=
                              V2P.Context.Navigation_Links.Get_Value
                                (Context.all, "Navigation_Links");
            Previous_Id   : constant String :=
                              V2P.Context.Previous (Selected_Post, TID);
            Next_Id       : constant String :=
                              V2P.Context.Next (Selected_Post, TID);
         begin
            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Forum_Entry.PREVIOUS, Previous_Id));

            if Previous_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Forum_Entry.PREVIOUS_THUMB,
                     Database.Get_Thumbnail (Previous_Id)));
            end if;

            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Forum_Entry.NEXT, Next_Id));

            if Next_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Forum_Entry.NEXT_THUMB,
                     Database.Get_Thumbnail (Next_Id)));
            end if;
         end Insert_Links;

         --  Insert the entry information

         Templates.Insert (Translations, Database.Get_Entry (TID));
      end if;
   end Forum_Entry_Callback;

   ----------------------------
   -- Forum_Threads_Callback --
   ----------------------------

   procedure Forum_Threads_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Translations);

      P    : constant Parameters.List := Status.Parameters (Request);
      FID  : constant String :=
               Parameters.Get (P, Template_Defs.Forum_Threads.HTTP.FID);
      From : Positive := 1;
   begin
      --  Set forum Id into the context

      Context.Set_Value (Template_Defs.Global.FID, FID);

      if Context.Exist (Template_Defs.Global.TID) then
         Context.Remove (Template_Defs.Global.TID);
      end if;

      if Parameters.Exist
        (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM)
      then
         From := Positive'Value
           (Parameters.Get (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM));
      end if;

      V2P.Context.Navigation_From.Set_Value
        (Context.all, Template_Defs.Global.NAV_FROM, From);

      V2P.Context.Context_Filter (Context);
   end Forum_Threads_Callback;

   ---------------------
   -- Get_Images_Path --
   ---------------------

   function Get_Images_Path return String is
   begin
      return Gwiad_Plugin_Path
        & Morzhol.OS.Directory_Separator & Settings.Get_Images_Path;
   end Get_Images_Path;

   ------------------------
   -- Main_Page_Callback --
   ------------------------

   procedure Main_Page_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist (Template_Defs.Global.TID) then
         Context.Remove (Template_Defs.Global.TID);
      end if;
      if Context.Exist (Template_Defs.Global.FID) then
         Context.Remove (Template_Defs.Global.FID);
      end if;

      V2P.Context.Context_Filter (Context);
   end Main_Page_Callback;

   ------------------------
   -- New_Photo_Callback --
   ------------------------

   procedure New_Photo_Callback
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use Image.Data;

      SID         : constant Session.Id := Status.Session (Request);
      P           : constant Parameters.List := Status.Parameters (Request);
      Login       : constant String :=
                      Session.Get (SID, Template_Defs.Global.LOGIN);
      Filename    : constant String :=
                      Parameters.Get
                        (P, Template_Defs.Block_New_Photo.HTTP.FILENAME);

      Images_Path : String renames Get_Images_Path;

      New_Image   : Image_Data;

   begin
      Init
        (Img      => New_Image,
         Root_Dir => Gwiad_Plugin_Path,
         Filename => Filename);

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
         Insert_Photo : declare
            New_Photo_Filename : constant String := New_Image.Filename
              (Images_Path'Length + 1 .. New_Image.Filename'Last);
            Pid                : constant String := Database.Insert_Photo
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
         end Insert_Photo;
      end if;
   end New_Photo_Callback;


   ---------------------
   -- Photos_Callback --
   ---------------------

   function Photos_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Gwiad_Plugin_Path & Directory_Separator &
                 Settings.Get_Images_Path & Directory_Separator
                   & URI
                      (URI'First +
                         Settings.Images_Source_Prefix'Length + 1 .. URI'Last);
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
         Web_JS_URI,
         Action => Dispatchers.Callback.Create (WEJS_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         CSS_URI,
         Action => Dispatchers.Callback.Create (CSS_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Images_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Thumbs_Source_Prefix,
         Action => Dispatchers.Callback.Create (Thumbs_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Dispatcher => Main_Dispatcher,
         URI        => Settings.Website_Data_Prefix,
         Action     => Dispatchers.Callback.Create (Website_Data'Access),
         Prefix     => True);

      Services.Dispatchers.URI.Register_Default_Callback
        (Main_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));
      --  This default callback will handle all Web_Block callbacks

      --  Register Web_Block pages

      Services.Web_Block.Registry.Register
        (Key          => Template_Defs.User_Page.URL,
         Template     => Template_Defs.User_Page.Template,
         Data_CB      => null,
         Prefix       => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Forum_Entry.URL,
         Template_Defs.Forum_Entry.Template,
         Forum_Entry_Callback'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Photo.URL,
         Template_Defs.Iframe_Photo_Post.Template,
         New_Photo_Callback'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Forum_Threads.URL,
         Template_Defs.Forum_Threads.Template,
         Forum_Threads_Callback'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Main_Page.URL,
         Template_Defs.Main_Page.Template,
         Main_Page_Callback'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Error.URL,
         Template_Defs.Error.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Forum_Post.URL,
         Template_Defs.Forum_Post.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Admin.URL,
         Template_Defs.Admin.Template,
         null);

      --  Register Ajax callbacks

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_login_form_enter,
         Template_Defs.R_Block_Login.Template,
         Web_Ajax_Callbacks.Login_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_logout_enter,
         Template_Defs.R_Block_Logout.Template,
         V2P.Web_Ajax_Callbacks.Logout_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Filter.Ajax.onchange_forum_filter_set,
         Template_Defs.R_Block_Forum_Filter.Template,
         V2P.Web_Ajax_Callbacks.Onchange_Filter_Forum'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Forum_Entry.Ajax.onclick_hidden_status_toggle,
         Template_Defs.R_Block_Hidden_Status.Template,
         V2P.Web_Ajax_Callbacks.Onclick_Hidden_Status_Toggle'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onchange_sel_forum_list,
         Template_Defs.R_Block_Forum_List.Template,
         V2P.Web_Ajax_Callbacks.Onchange_Forum_List_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onsubmit_comment_form,
         Template_Defs.R_Block_Comment_Form_Enter.Template,
         V2P.Web_Ajax_Callbacks.Onsubmit_Comment_Form_Enter_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Post.Ajax.onsubmit_post_form,
         Template_Defs.R_Block_Post_Form_Enter.Template,
         V2P.Web_Ajax_Callbacks.Onsubmit_Post_Form_Enter_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Metadata.Ajax.onsubmit_metadata_post,
         Template_Defs.R_Block_Metadata_Form_Enter.Template,
         V2P.Web_Ajax_Callbacks.Onsubmit_Metadata_Form_Enter_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Page.Ajax.onsubmit_user_page_edit_form,
         Template_Defs.R_Block_User_Page_Edit_Form_Enter.Template,
         Web_Ajax_Callbacks.Onsubmit_User_Page_Edit_Form_Enter_Callback'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (XML_Prefix_URI,
         Default_XML_Callback'Access,
         null,
         Content_Type => MIME.Text_XML);
      --  All URLs starting with XML_Prefix_URI are handled by a specific
      --  callback returning the corresponding file in the xml directory.
   end Start;

   ---------------------
   -- Thumbs_Callback --
   ---------------------

   function Thumbs_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Gwiad_Plugin_Path & Directory_Separator &
                 Settings.Get_Thumbs_Path & Directory_Separator
                   & URI
                      (URI'First +
                         Settings.Thumbs_Source_Prefix'Length + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (File), File);
   end Thumbs_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      pragma Unreferenced (Name);
   begin
      Gwiad.Web.Virtual_Host.Unregister (Settings.Virtual_Host);
   end Unregister;

   ------------------
   -- Website_Data --
   ------------------

   function Website_Data (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Gwiad_Plugin_Path & Directory_Separator &
                 Settings.Website_Data_Path & Directory_Separator
                   & URI
                      (URI'First +
                         Settings.Website_Data_Prefix'Length + 1 .. URI'Last);
   begin
      return Response.File
        (Content_Type => MIME.Content_Type (File), Filename => File);
   end Website_Data;

   -------------------
   -- WEJS_Callback --
   -------------------

   function WEJS_Callback (Request : in Status.Data) return Response.Data is
      URI          : constant String := Status.URI (Request);
      File         : constant String := Gwiad_Plugin_Path
        & Directory_Separator & URI (URI'First + 1 .. URI'Last);
      Translations : Templates.Translate_Set;
   begin
      return Response.Build
        (MIME.Content_Type (File),
         String'(Templates.Parse (File, Translations)));
   end WEJS_Callback;

begin  -- V2P.Web_Server : register vision2pixels website
   Start;

   Gwiad.Web.Virtual_Host.Register
     (Hostname => Settings.Virtual_Host,
      Action   => Main_Dispatcher);

   Gwiad.Plugins.Websites.Registry.Register
     (Name         => "vision2pixels",
      Description  => "a Web space engine to comment user's photos",
      Unregister   => Unregister'Access,
      Library_Path => V2p_Lib_Path);
end V2P.Web_Server;
