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
with V2P.Template_Defs.Global;
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Error;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_Forum_Navigate;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_Forum_List;
with V2P.Template_Defs.R_Block_Forum_Filter;

with Image.Data;
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

   function New_Comment_Callback
     (Request : in Status.Data) return Response.Data;
   --  Enter a new comment into the database

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

      if Session.Get (SID, "FID") /= "" then
         --  ??? needs to be put inside the right ECVW callback
         Templates.Insert
           (Translations,
            Templates.Assoc
              ("Current_FID", String'(Session.Get (SID, "FID"))));
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
--        FID         : constant String :=
--                     Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.FID);
      FID         : constant String := Context.Get_Value ("FID");
      Login       : constant String := Session.Get (SID, "LOGIN");
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the session
      Context.Set_Value ("TID", TID);
      Context.Set_Value ("FID", FID);

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
      --  Set forum Id into the session
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

   --------------------------
   -- New_Comment_Callback --
   --------------------------

   function New_Comment_Callback
     (Request : in Status.Data) return Response.Data
   is
      use Image.Data;

      SID          : constant Session.Id := Status.Session (Request);
      P            : constant Parameters.List := Status.Parameters (Request);
      Login        : constant String := Session.Get (SID, "LOGIN");
--        TID          : constant String := Parameters.Get (P, "TID");
--        FID          : constant String := Parameters.Get (P, "FID");
      Anonymous    : constant String := Parameters.Get (P, "ANONYMOUS_USER");
      Name         : constant String := Parameters.Get (P, "NAME");
      Comment      : constant String := Parameters.Get (P, "COMMENT");
      Filename     : constant String := Parameters.Get (P, "FILENAME");
      CID          : constant String := Parameters.Get (P, "CATEGORY");
      Forum        : constant String := Parameters.Get (P, "FORUM");
      Context      : constant String := Parameters.Get (P, "CTX_ECWF");
      Context_Id   : constant Services.ECWF.Context.Id :=
                       Services.ECWF.Context.Value (Context);
      Ctx          : constant Services.ECWF.Context.Object :=
                       Services.ECWF.Context.Get (Context_Id);
      TID          : constant String := Ctx.Get_Value ("TID");
      FID          : constant String := Ctx.Get_Value ("FID");
      pragma Unreferenced (Context_Id);
      Images_Path  : String renames Settings.Get_Images_Path;

      New_Image    : Image_Data;
      Translations : Templates.Translate_Set;

   begin
      if (Login = "" and then Anonymous = "")
        or else
          not Is_Valid_Comment (Comment)
      then
         return Response.URL
           (Location => Template_Defs.Forum_Entry.URL & "?TID=" & TID
            & "&FID=" & FID);
      end if;

      if Filename /= "" then

         Init (New_Image, Filename, Database.Get_Category_Full_Name (CID));

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

            return Services.ECWF.Registry.Build
              (Template_Defs.Main_Page.URL, Request, Translations);
         end if;
      end if;

      if TID = "" then
         if Filename /= "" then
            Database.Insert_Post
              (Login, CID, Name, Comment,
               New_Image.Filename
                 ((Images_Path'Length + 2) .. New_Image.Filename'Last),
               New_Image.Width,
               New_Image.Height,
               New_Image.Size);

         else
            Database.Insert_Post (Login, CID, Name, Comment);
         end if;

         return Response.URL
           (Location => Template_Defs.Forum_Threads.URL & "?FID=" & Forum);

      else
         Database.Insert_Comment
           (Login, Anonymous, TID, Name, Comment,
            Image.Data.Filename (New_Image));
         return Response.URL
           (Location => Template_Defs.Forum_Entry.URL & "?TID=" & TID
            & "&FID=" & FID);
      end if;
   end New_Comment_Callback;

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
      --  Using standard callbacks for full control over the response

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/xml_",
         Action => Dispatchers.Callback.Create (Default_Xml_Callback'Access),
         Prefix => True);
      --  All URLs starting with /xml_ are handled by a specific callback
      --  returning the corresponding file in the xml directory.

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_New_Comment.URL,
         Action => Dispatchers.Callback.Create (New_Comment_Callback'Access));

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
        (Template_Defs.Forum_Entry.URL,
         Template_Defs.Forum_Entry.Template,
         Forum_Entry_Callback'Access);

      Services.ECWF.Registry.Register
        (Template_Defs.Forum_Post.URL,
         Template_Defs.Forum_Post.Template,
         null);

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

      --  Register ECWF lazy tags

      Template_Defs.Lazy.Register;

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
