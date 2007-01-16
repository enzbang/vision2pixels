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
with AWS.Session;
with AWS.Status;
with AWS.Templates;

with V2P.Database;
with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Forum_Post;
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Error;
with V2P.Template_Defs.User;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_Quick_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Post;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Forum_Select;
with V2P.Template_Defs.Block_Forum_Threads;
with V2P.Template_Defs.Block_User_Password_Change;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_Forum_List;

with Image.Data;
with Settings;

package body V2P.Web_Server is

   use AWS;

   Null_Set : Templates.Translate_Set;

   HTTP            : Server.HTTP;
   Configuration   : Config.Object;
   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   function Default_Xml_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback for xml action

   function Forum_Entry_Callback
     (Request : in Status.Data) return Response.Data;
   --  Forum entry callback

   function Forum_Post_Callback
     (Request : in Status.Data) return Response.Data;
   --  Forum post callback

   function Forum_Threads_Callback
     (Request : in Status.Data) return Response.Data;
   --  Forum threads callback

   function Login_Callback (Request : in Status.Data) return Response.Data;
   --  Login callback

   function Logout_Callback (Request : in Status.Data) return Response.Data;
   --  Logout callback

   function User_Callback (Request : in Status.Data) return Response.Data;
   --  User (homepage) callback

   function User_Password_Change_Callback
     (Request : in Status.Data) return Response.Data;
   --  User password change callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   function CSS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element CSS callback

   function Photos_Callback (Request : in Status.Data) return Response.Data;
   --  Photos callback

   function Thumbs_Callback (Request : in Status.Data) return Response.Data;
   --  Thumbs callback

   function Onchange_Forum_List_Callback
     (Request : in Status.Data) return Response.Data;
   --  Called when a new forum is selected

   function Main_Page_Callback
     (Request : in Status.Data) return Response.Data;
   --  Display v2p main page

   function Error_Callback
     (Request : in Status.Data) return Response.Data;
   --  Error callback

   function New_Comment_Callback
     (Request : in Status.Data) return Response.Data;
   --  Enter a new comment into the database

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set;
      Filename_Type     : in String := MIME.Text_HTML) return Response.Data;
   --  Parsing routines used for all V2P templates. This routine add supports
   --  for lazy tags.

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

   --------------------
   -- Error_Callback --
   --------------------

   function Error_Callback
     (Request : in Status.Data) return Response.Data
   is
      Translations : Templates.Translate_Set;
   begin
      --  ??? Should return 404 Error
      return Final_Parse
        (Request,
         Template_Defs.Error.Template,
         Translations);
   end Error_Callback;

   -----------------
   -- Final_Parse --
   -----------------

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set;
      Filename_Type     : in String := MIME.Text_HTML) return Response.Data
   is
      SID : constant Session.Id := Status.Session (Request);

      type Lazy_Tags is new Templates.Dynamic.Lazy_Tag with record
         Translations : Templates.Translate_Set;
      end record;

      overriding procedure Value
        (Lazy_Tag     : not null access Lazy_Tags;
         Var_Name     : in String;
         Translations : in out Templates.Translate_Set);
      --  ??

      Final_Translations : Templates.Translate_Set := Translations;

      LT  : aliased Lazy_Tags := (Templates.Dynamic.Lazy_Tag with
                                  Translations => Final_Translations);

      -----------
      -- Value --
      -----------

      procedure Value
        (Lazy_Tag     : not null access Lazy_Tags;
         Var_Name     : in String;
         Translations : in out Templates.Translate_Set)
      is
         Local_Translations : Templates.Translate_Set := Lazy_Tag.Translations;
      begin
         if Var_Name = Template_Defs.Lazy.Login then
            Templates.Insert
              (Local_Translations,
               Database.Get_User (Session.Get (SID, "LOGIN")));

            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Login,
                 String'(Templates.Parse
                   (Template_Defs.Block_Login.Template, Local_Translations))));

         elsif Var_Name = Template_Defs.Lazy.Forum_List then
            Templates.Insert (Local_Translations, Database.Get_Forums);
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Forum_List,
                 String'(Templates.Parse
                   (Template_Defs.Block_Forum_List.Template,
                      Local_Translations))));

         elsif Var_Name = Template_Defs.Lazy.Forum_List_Select then
            Templates.Insert (Local_Translations, Database.Get_Forums);
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Forum_List_Select,
                 String'(Templates.Parse
                   (Template_Defs.Block_Forum_Select.Template,
                      Local_Translations))));

         elsif Var_Name = Template_Defs.Lazy.User_Thread_List then
            if Session.Get (SID, "LOGIN") /= "" then
               Templates.Insert (Local_Translations,
                                 Database.Get_Threads
                                   (User => Session.Get (SID, "LOGIN")));

               Templates.Insert
                 (Translations,
                  Templates.Assoc (Template_Defs.Lazy.Forum_Threads,
                    String'(Templates.Parse
                      (Template_Defs.Block_Forum_Threads.Template,
                         Local_Translations))));
            end if;

         elsif Var_Name = Template_Defs.Lazy.Forum_Threads then
            if Session.Get (SID, "FID") /= "" then
               Templates.Insert
                 (Local_Translations,
                  Database.Get_Threads (Fid => Session.Get (SID, "FID")));

               Templates.Insert
                 (Translations,
                  Templates.Assoc (Template_Defs.Lazy.Forum_Threads,
                    String'(Templates.Parse
                      (Template_Defs.Block_Forum_Threads.Template,
                         Local_Translations))));
            end if;

         elsif Var_Name = Template_Defs.Lazy.Quick_Login then
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Quick_Login,
                 String'(Templates.Parse
                   (Template_Defs.Block_Quick_Login.Template,
                      Local_Translations))));

         elsif Var_Name = Template_Defs.Lazy.New_Comment then
            if Session.Exist (SID, "FID") then

               Templates.Insert
                 (Local_Translations,
                  Database.Get_Categories (Session.Get (SID, "FID")));
            end if;

            if Session.Exist (SID, "TID") then
               Templates.Insert
                 (Local_Translations,
                  Templates.Assoc
                    (Template_Defs.Block_New_Comment.Current_Tid,
                     String'(Session.Get (SID, "TID"))));
            end if;

            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.New_Comment,
                 String'(Templates.Parse
                   (Template_Defs.Block_New_Comment.Template,
                      Local_Translations,
                      Lazy_Tag => LT'Unchecked_Access))));
         elsif Var_Name = Template_Defs.Lazy.New_Post then
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.New_Post,
                 String'(Templates.Parse
                   (Template_Defs.Block_New_Post.Template,
                      Local_Translations,
                      Lazy_Tag => LT'Unchecked_Access))));
         end if;
      end Value;

   begin
      Templates.Insert
        (Final_Translations,
         Templates.Assoc ("LOGIN", String'(Session.Get (SID, "LOGIN"))));

      if Session.Get (SID, "FID") /= "" then
         Templates.Insert
           (Final_Translations,
            Templates.Assoc ("Current_FID",
              String'(Session.Get (SID, "FID"))));
      end if;

      --  Adds some URL

      Templates.Insert
        (Final_Translations,
         Templates.Assoc
           ("FORUM_THREAD_URL", Template_Defs.Forum_Threads.URL));
      Templates.Insert
        (Final_Translations,
         Templates.Assoc ("FORUM_POST_URL", Template_Defs.Forum_Post.URL));
      Templates.Insert
        (Final_Translations,
         Templates.Assoc ("FORUM_ENTRY_URL", Template_Defs.Forum_Entry.URL));

      return Response.Build
        (Filename_Type,
         String'(Templates.Parse
           (Template_Filename,
              Final_Translations,
              Lazy_Tag => LT'Unchecked_Access)),
         Cache_Control => Messages.Prevent_Cache);
   end Final_Parse;

   --------------------------
   -- Forum_Entry_Callback --
   --------------------------

   function Forum_Entry_Callback
     (Request : in Status.Data) return Response.Data
   is
      SID         : constant Session.Id := Status.Session (Request);
      P           : constant Parameters.List := Status.Parameters (Request);
      TID         : constant String :=
                      Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.Tid);
      Logged_User : constant String := Session.Get (SID, "LOGIN");
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the session
      Session.Set (SID, "TID", TID);

      if not Settings.Anonymous_Visit_Counter then
         --  Do not count anonymous click
         if Logged_User = "" then
            Count_Visit := False;

         else
            if Settings.Ignore_Author_Click
              and then Database.Is_Author (Logged_User, TID)
            then
               --  Do not count author click
               Count_Visit := False;
            end if;
         end if;
      end if;

      if Count_Visit then
         Database.Increment_Visit_Counter (TID);
      end if;

      return Final_Parse
        (Request,
         Template_Defs.Forum_Entry.Template,
         Database.Get_Entry (TID));
   end Forum_Entry_Callback;

   -------------------------
   -- Forum_Post_Callback --
   -------------------------

   function Forum_Post_Callback
     (Request : in Status.Data) return Response.Data is
   begin
      return Final_Parse
        (Request, Template_Defs.Forum_Post.Template, Null_Set);
   end Forum_Post_Callback;

   ----------------------------
   -- Forum_Threads_Callback --
   ----------------------------

   function Forum_Threads_Callback
     (Request : in Status.Data) return Response.Data
   is
      SID : constant Session.Id := Status.Session (Request);
      P   : constant Parameters.List := Status.Parameters (Request);
      FID : constant String :=
              Parameters.Get (P, Template_Defs.Forum_Threads.HTTP.Fid);
   begin
      --  Set forum Id into the session
      Session.Set (SID, "FID", FID);
      if Session.Exist (SID, "TID") then
         Session.Remove (SID, "TID");
      end if;

      return Final_Parse
        (Request,
         Template_Defs.Forum_Threads.Template,
         Database.Get_Threads (FID));
   end Forum_Threads_Callback;

   --------------------
   -- Login_Callback --
   --------------------

   function Login_Callback (Request : in Status.Data) return Response.Data is
      SID      : constant Session.Id := Status.Session (Request);
      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String := Parameters.Get (P, "LOGIN");
      Password : constant String := Database.Get_Password (Login);

      Set      : Templates.Translate_Set;
   begin
      if Password = Parameters.Get (P, "PASSWORD") then
         Session.Set (SID, "LOGIN", Login);
         Session.Set (SID, "PASSWORD", Password);

         Templates.Insert (Set, Templates.Assoc
                             (Template_Defs.R_Block_Login.Login,
                              String'(Session.Get (SID, "LOGIN"))));

         return Final_Parse (Request, Template_Defs.R_Block_Login.Template,
                             Set, MIME.Text_XML);
      else
         return Response.Build
           (MIME.Text_XML,
            String'(Templates.Parse (Template_Defs.R_Block_Login.Template)));
      end if;
   end Login_Callback;

   ---------------------
   -- Logout_Callback --
   ---------------------

   function Logout_Callback (Request : in Status.Data) return Response.Data is
      SID : constant Session.Id := Status.Session (Request);
      Set : Templates.Translate_Set;
   begin
      Session.Delete (SID);

      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.R_Block_Logout.Login_Form,
         String'(Templates.Parse
           (Template_Defs.Block_Login.Template))));

      return Final_Parse
        (Request, Template_Defs.R_Block_Logout.Template,
         Set, MIME.Text_XML);
   end Logout_Callback;

   -----------------------
   -- Main_Page_Callback --
   -----------------------

   function Main_Page_Callback
     (Request : in Status.Data) return Response.Data
   is
      SID          : constant Session.Id := Status.Session (Request);
      Translations : Templates.Translate_Set;
   begin
      --  Main page, remove the current session status
      if Session.Exist (SID, "TID") then
         Session.Remove (SID, "TID");
      end if;
      if Session.Exist (SID, "FID") then
         Session.Remove (SID, "FID");
      end if;

      return Final_Parse
        (Request,
         Template_Defs.Main_Page.Template,
         Translations);
   end Main_Page_Callback;

   --------------------------
   -- New_Comment_Callback --
   --------------------------

   function New_Comment_Callback
     (Request : in Status.Data) return Response.Data
   is
      use Image.Data;

      SID       : constant Session.Id := Status.Session (Request);
      P         : constant Parameters.List := Status.Parameters (Request);
      Login     : constant String := Session.Get (SID, "LOGIN");
      TID       : constant String := Parameters.Get (P, "TID");
      Anonymous : constant String := Parameters.Get (P, "ANONYMOUS_USER");
      Name      : constant String := Parameters.Get (P, "NAME");
      Comment   : constant String := Parameters.Get (P, "COMMENT");
      Filename  : constant String := Parameters.Get (P, "FILENAME");
      CID       : constant String := Parameters.Get (P, "CATEGORY");
      Forum     : constant String := Parameters.Get (P, "FORUM");

      Images_Path : constant String := Settings.Get_Images_Path;

      New_Image : Image_Data;

      Translations : Templates.Translate_Set;

   begin

      if Filename /= "" then

         Init (New_Image, Filename, Database.Get_Category_Full_Name (CID));

         if New_Image.Init_Status /= Image_Created then
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Main_Page.V2p_Error,
                 Image_Init_Status'Image (New_Image.Init_Status)));

            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.Main_Page.Exceed_Maximum_Image_Dimension,
                  Image_Init_Status'Image
                    (Image.Data.Exceed_Max_Image_Dimension)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Main_Page.Exceed_Maximum_Size,
                  Image_Init_Status'Image
                    (Image.Data.Exceed_Max_Size)));

            return Final_Parse
              (Request,
               Template_Defs.Main_Page.Template,
               Translations);
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
           (Location => Template_Defs.Forum_Entry.URL & "?TID=" & TID);
      end if;
   end New_Comment_Callback;

   ----------------------------------
   -- Onchange_Forum_List_Callback --
   ----------------------------------

   function Onchange_Forum_List_Callback
     (Request : in Status.Data) return Response.Data
   is
      P   : constant Parameters.List := Status.Parameters (Request);
      Fid : constant String := Parameters.Get (P, "sel_forum_list");
      --  ??
   begin
      return Response.Build
        (MIME.Text_XML,
         String'(Templates.Parse
           (Template_Defs.R_Block_Forum_List.Template,
              Database.Get_Categories (Fid))));
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
      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/xml_",
         Action => Dispatchers.Callback.Create (Default_Xml_Callback'Access),
         Prefix => True);
      --  All URLs starting with /xml_ are handled by a specific callback
      --  returning the corresponding file in the xml directory.

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_Login.Ajax.Onclick_Login_Form_Enter,
         Action => Dispatchers.Callback.Create (Login_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_Login.Ajax.Onclick_Logout_Enter,
         Action => Dispatchers.Callback.Create (Logout_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_New_Comment.Ajax.Onchange_Sel_Forum_List,
         Action => Dispatchers.Callback.Create
           (Onchange_Forum_List_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_User_Password_Change.
           Ajax.Onclick_User_Password_Change_Enter,
         Action => Dispatchers.Callback.Create
           (User_Password_Change_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_New_Comment.URL,
         Action => Dispatchers.Callback.Create (New_Comment_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Forum_Entry.URL,
         Action => Dispatchers.Callback.Create (Forum_Entry_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Forum_Post.URL,
         Action => Dispatchers.Callback.Create (Forum_Post_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Forum_Threads.URL,
         Action => Dispatchers.Callback.Create
           (Forum_Threads_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.User.URL,
         Action => Dispatchers.Callback.Create
           (User_Callback'Access));

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

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Main_Page.URL,
         Action => Dispatchers.Callback.Create (Main_Page_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/",
         Action => Dispatchers.Callback.Create (Error_Callback'Access),
         Prefix => True);

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

   -------------------
   -- User_Callback --
   -------------------

   function User_Callback (Request : in Status.Data) return Response.Data is
      SID          : constant Session.Id := Status.Session (Request);
      Translations : Templates.Translate_Set;
   begin
      --  User page, remove the current session status
      if Session.Exist (SID, "TID") then
         Session.Remove (SID, "TID");
      end if;
      if Session.Exist (SID, "FID") then
         Session.Remove (SID, "FID");
      end if;

      return Final_Parse
        (Request,
         Template_Defs.User.Template, Translations);
   end User_Callback;

   -----------------------------------
   -- User_Password_Change_Callback --
   -----------------------------------

   function User_Password_Change_Callback
     (Request : in Status.Data) return Response.Data is
   begin
      pragma Unreferenced (Request);
      return Response.Build (MIME.Text_XML, "");
   end User_Password_Change_Callback;

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
      URI  : constant String := Status.URI (Request);
      File : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (File), File);
   end WEJS_Callback;

end V2P.Web_Server;
