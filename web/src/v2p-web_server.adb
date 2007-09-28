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

with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Registry;
with AWS.Session;
with AWS.Status;
with AWS.Templates;

with Gwiad.Web.Virtual_Host;
with Gwiad.Plugins.Websites.Registry;
with Morzhol.OS;

with V2P.Settings;

with V2P.Callbacks.Page;
with V2P.Callbacks.Ajax;
with V2P.Logs;

with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Admin;
with V2P.Template_Defs.Page_User;
with V2P.Template_Defs.Page_Main;
with V2P.Template_Defs.Page_Forum_New_Entry;
with V2P.Template_Defs.Page_Error;
with V2P.Template_Defs.Set_Global;
with V2P.Template_Defs.Page_Photo_Post;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
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

with Gwiad.Plugins.Websites;

package body V2P.Web_Server is

   use Ada;
   use AWS;

   use Morzhol.OS;

   use AWS.Services.Web_Block.Registry;
   use Gwiad.Plugins.Websites;

   Module          : constant Logs.Module_Name := "V2P.Web_Server";

   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   XML_Path        : constant String :=
                       Directories.Compose
                         (Containing_Directory => Gwiad_Plugin_Path,
                          Name                 => "xml");
   XML_Prefix_URI  : constant String := "/xml_";
   CSS_URI         : constant String := "/css";
   Web_JS_URI      : constant String := "/we_js";

   V2p_Lib_Path    : constant String :=
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

   -------------
   --  Gwiad  --
   -------------

   procedure Unregister (Name : in Website_Name);
   --  Unregister website

   --------------------------
   -- Other local routines --
   --------------------------

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
         Templates.Assoc (Template_Defs.Set_Global.LOGIN,
           String'(Session.Get (SID, Template_Defs.Set_Global.LOGIN))));
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
      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Set_Global.LOGIN,
               String'(Session.Get (SID, Template_Defs.Set_Global.LOGIN))));
      end if;

      if Session.Exist (SID, Template_Defs.Set_Global.ADMIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Set_Global.ADMIN,
               String'(Session.Get (SID, Template_Defs.Set_Global.ADMIN))));
      end if;

      --  Adds Version number

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.V2P_VERSION,
            V2P.Version));

      --  Adds some URL

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_THREAD_URL,
            Template_Defs.Page_Forum_Threads.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_POST_URL,
            Template_Defs.Page_Forum_New_Entry.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_NEW_PHOTO_URL,
            Template_Defs.Page_Photo_Post.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.ADMIN_URL,
            Template_Defs.Page_Admin.URL));

      --  Insert global options

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.OPTION_ANONYMOUS_COMMENT,
            Settings.Anonymous_Comment));

      --  Insert the images prefixes

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.THUMB_SOURCE_PREFIX,
            Settings.Thumbs_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.IMAGE_SOURCE_PREFIX,
            Settings.Images_Source_Prefix));

      Web_Page := Services.Web_Block.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         Web_Page := Services.Web_Block.Registry.Build
           (Template_Defs.Page_Error.URL, Request, Translations);
      end if;

      return Web_Page;

   exception
      when others =>
         Fatal_Error : begin
            if
              Services.Web_Block.Registry.Content_Type (URI) = MIME.Text_HTML
            then
               Logs.Write
                 (Module, Logs.Error, "Default_Callback HTML exception for "
                    & Logs.NV ("URI", URI));
               return Response.Build
                 (Message_Body => "<p>Internal error</p>",
                  Content_Type => MIME.Text_HTML);

            else
               --  ??? In this case we probably want to add some Ajax error
               --  report.
               Logs.Write
                 (Module, Logs.Error, "Default_Callback XML exception for "
                    & Logs.NV ("URI", URI));
               return Response.Build
                 (Message_Body => "<p>Internal error</p>",
                  Content_Type => MIME.Text_XML);
            end if;
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

   ------------------------
   -- Register_Callbacks --
   ------------------------

   procedure Register_Callbacks is
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
        (Key      => Template_Defs.Page_User.URL,
         Template => Template_Defs.Page_User.Template,
         Data_CB  => null,
         Prefix   => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Entry.URL,
         Template_Defs.Page_Forum_Entry.Template,
         Callbacks.Page.Forum_Entry'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Threads.URL,
         Template_Defs.Page_Forum_Threads.Template,
         Callbacks.Page.Forum_Threads'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Main.URL,
         Template_Defs.Page_Main.Template,
         Callbacks.Page.Main'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Error.URL,
         Template_Defs.Page_Error.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Admin.URL,
         Template_Defs.Page_Admin.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Photo_Post.URL,
         Template_Defs.Page_Photo_Post.Template,
         Callbacks.Page.Post_Photo'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Entry.URL,
         Template_Defs.Page_Forum_New_Entry.Template,
         Callbacks.Page.New_Entry'Access);

      --  Register Ajax callbacks

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_login_form_enter,
         Template_Defs.R_Block_Login.Template,
         Callbacks.Ajax.Login'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_logout_enter,
         Template_Defs.R_Block_Logout.Template,
         Callbacks.Ajax.Logout'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Filter.Ajax.onchange_forum_filter_set,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onchange_Filter_Forum'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Entry.Ajax.onclick_hidden_status_toggle,
         Template_Defs.R_Block_Hidden_Status.Template,
         Callbacks.Ajax.Onclick_Hidden_Status_Toggle'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onchange_sel_forum_list,
         Template_Defs.R_Block_Forum_List.Template,
         Callbacks.Ajax.Onchange_Forum_List'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onsubmit_comment_form,
         Template_Defs.R_Block_Comment_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Comment_Form_Enter'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Entry.
           Ajax.onsubmit_new_entry_form_submit,
         Template_Defs.R_Block_Post_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Post_Form_Enter'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Metadata.Ajax.onsubmit_metadata_post,
         Template_Defs.R_Block_Metadata_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Metadata_Form_Enter'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Page.Ajax.onsubmit_user_page_edit_form,
         Template_Defs.R_Block_User_Page_Edit_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_User_Page_Edit_Form_Enter'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (XML_Prefix_URI,
         Default_XML_Callback'Access,
         null,
         Content_Type => MIME.Text_XML);
      --  All URLs starting with XML_Prefix_URI are handled by a specific
      --  callback returning the corresponding file in the xml directory.
   end Register_Callbacks;

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

   Register_Callbacks;

   Gwiad.Web.Virtual_Host.Register
     (Hostname => Settings.Virtual_Host,
      Action   => Main_Dispatcher);

   Gwiad.Plugins.Websites.Registry.Register
     (Name         => "vision2pixels",
      Description  => "a Web space engine to comment user's photos",
      Unregister   => Unregister'Access,
      Library_Path => V2p_Lib_Path);
end V2P.Web_Server;
