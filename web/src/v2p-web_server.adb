------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_New_Comment;

with Settings;

package body V2P.Web_Server is

   use AWS;

   HTTP            : Server.HTTP;
   Configuration   : Config.Object;
   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   function Forum_Callback (Request : in Status.Data) return Response.Data;
   --  Forum callback

   function Login_Callback (Request : in Status.Data) return Response.Data;
   --  Login callback

   function Logout_Callback (Request : in Status.Data) return Response.Data;
   --  Logout callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   function CSS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element CSS callback

   function Photos_Callback (Request : in Status.Data) return Response.Data;
   --  Photos callback

   function Main_Page_Callback
     (Request : in Status.Data) return Response.Data;
   --  Display v2p main page

   function New_Comment_Callback
     (Request : in Status.Data) return Response.Data;
   --  Enter a new comment into the database

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set) return Response.Data;
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


   -----------------
   -- Final_Parse --
   -----------------

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set) return Response.Data
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
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Forum_List,
                 String'(Templates.Parse
                     (Template_Defs.Block_Forum_List.Template,
                      Database.Get_Forums))));

         elsif Var_Name = Template_Defs.Lazy.New_Comment then
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.New_Comment,
                 String'(Templates.Parse
                   (Template_Defs.Block_New_Comment.Template,
                      Local_Translations))));
         end if;
      end Value;

      Final_Translations : Templates.Translate_Set := Translations;

      LT : aliased Lazy_Tags := (Templates.Dynamic.Lazy_Tag with
                                 Translations => Final_Translations);

   begin
      Templates.Insert
        (Final_Translations,
         Templates.Assoc ("LOGIN", String'(Session.Get (SID, "LOGIN"))));

      return Response.Build
        (MIME.Text_HTML,
         String'(Templates.Parse
           (Template_Filename,
              Final_Translations,
              Lazy_Tag => LT'Unchecked_Access)),
         Cache_Control => Messages.Prevent_Cache);
   end Final_Parse;

   --------------------
   -- Forum_Callback --
   --------------------

   function Forum_Callback (Request : in Status.Data) return Response.Data is
      SID : constant Session.Id := Status.Session (Request);
      URI : constant String := Status.URI (Request);
      P   : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/forum/threads" then
         declare
            FID : constant String :=
                    Parameters.Get (P, Template_Defs.Forum_Threads.HTTP.Fid);
         begin
            --  Set forum Id into the session
            Session.Set (SID, "FID", FID);
            return Final_Parse
              (Request,
               Template_Defs.Forum_Threads.Template,
               Database.Get_Threads (FID));
         end;

      elsif URI = "/forum/entry" then
         declare
            TID : constant String :=
                    Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.Tid);
         begin
            --  Set thread Id into the session
            Session.Set (SID, "TID", TID);

            if Settings.Anonymous_Visit_Counter or
              (not Settings.Anonymous_Visit_Counter
               and String'(Session.Get (SID, "LOGIN")) /= "") then
               Database.Increment_Visit_Counter (TID);
            end if;
            return Final_Parse
              (Request,
               Template_Defs.Forum_Entry.Template,
               Database.Get_Entry (TID));
         end;
      end if;

      return Response.Build (MIME.Text_HTML, "not found!");
   end Forum_Callback;

   --------------------
   -- Login_Callback --
   --------------------

   function Login_Callback (Request : in Status.Data) return Response.Data is
      SID      : constant Session.Id := Status.Session (Request);
      P        : constant Parameters.List := Status.Parameters (Request);
      Login    : constant String := Parameters.Get (P, "LOGIN");
      Password : constant String := Database.Get_Password (Login);
   begin
      if Password = Parameters.Get (P, "PASSWORD") then
         Session.Set (SID, "LOGIN", Login);
         Session.Set (SID, "PASSWORD", Password);

         return Response.Build
           (MIME.Text_XML,
            String'(Templates.Parse
              (Template_Defs.R_Block_Login.Template,
                 (1 => Templates.Assoc (Template_Defs.R_Block_Login.Login,
                  String'(Session.Get (SID, "LOGIN"))),
                  2 => Templates.Assoc (Template_Defs.R_Block_Login.Login_Form,
                    String'(Templates.Parse
                      (Template_Defs.Block_Login.Template,
                         (1 => Templates.Assoc
                            (Template_Defs.Block_Login.Login,
                             String'(Session.Get (SID, "LOGIN")))))))))));
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
   begin
      Session.Delete (SID);

      return Response.Build
        (MIME.Text_XML,
         String'(Templates.Parse
           (Template_Defs.R_Block_Logout.Template,
              (1 => Templates.Assoc (Template_Defs.R_Block_Logout.Login_Form,
               String'(Templates.Parse
                 (Template_Defs.Block_Login.Template)))))));
   end Logout_Callback;

   -----------------------
   -- Main_Page_Callback --
   -----------------------

   function Main_Page_Callback
     (Request : in Status.Data) return Response.Data
   is
      Translations : Templates.Translate_Set;
   begin
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
      SID     : constant Session.Id := Status.Session (Request);
      P       : constant Parameters.List := Status.Parameters (Request);
      Login   : constant String := Session.Get (SID, "LOGIN");
      FID     : constant String := Session.Get (SID, "FID");
      TID     : constant String := Session.Get (SID, "TID");
      Name    : constant String := Parameters.Get (P, "NAME");
      Comment : constant String := Parameters.Get (P, "COMMENT");
   begin
      Database.Insert_Comment (Login, FID, TID, Name, Comment);

      return Response.Build
        (MIME.Text_XML,
         String'(Templates.Parse
           (Template_Defs.R_Block_New_Comment.Template,
              (1 => Templates.Assoc
                 (Template_Defs.R_Block_New_Comment.Tid, TID)))),
         Cache_Control => Messages.Prevent_Cache);
   end New_Comment_Callback;

   ---------------------
   -- Photos_Callback --
   ---------------------

   function Photos_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Settings.Get_Images_Path & "/"
                 & URI (URI'First +
                          Image_Source_Prefix'Length + 1 .. URI'Last);
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
         Template_Defs.Block_Login.Ajax.Onclick_Login_Form_Enter,
         Action => Dispatchers.Callback.Create (Login_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_Login.Ajax.Onclick_Logout_Enter,
         Action => Dispatchers.Callback.Create (Logout_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Template_Defs.Block_New_Comment.Ajax.Onclick_Comment_Form_Enter,
         Action => Dispatchers.Callback.Create (New_Comment_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/forum",
         Action => Dispatchers.Callback.Create (Forum_Callback'Access),
         Prefix => True);

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
         Image_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/",
         Action => Dispatchers.Callback.Create (Main_Page_Callback'Access),
         Prefix => True);


      Server.Log.Start (HTTP, Auto_Flush => True);

      Server.Log.Start_Error (HTTP);

      Config.Set.Session (Configuration, True);
      Config.Set.Admin_URI (Configuration, "/admin");

      Server.Start (HTTP, Main_Dispatcher, Configuration);

      Server.Wait (Server.Forever);
   end Start;

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
