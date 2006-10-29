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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
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
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.R_Block_Login;

package body V2P.Web_Server is

   use AWS;

   HTTP            : Server.HTTP;
   Configuration   : Config.Object;
   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   function Forum_Callback (Request : in Status.Data) return Response.Data;
   --  Forum callback

   function Login_Callback (Request : in Status.Data) return Response.Data;
   --  Login callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set) return Response.Data;
   --  Parsing routines used for all V2P templates. This routine add supports
   --  for lazy tags.

   -----------------
   -- Final_Parse --
   -----------------

   function Final_Parse
     (Request           : in Status.Data;
      Template_Filename : in String;
      Translations      : in Templates.Translate_Set) return Response.Data
   is
      SID : constant Session.Id := Status.Session (Request);

      type Lazy_Tags is new Templates.Dynamic.Lazy_Tag with null record;

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
         pragma Unreferenced (Lazy_Tag);
      begin
         if Var_Name = Template_Defs.Lazy.Login then
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Lazy.Login,
                 String'(Templates.Parse
                   (Template_Defs.Block_Login.Template,
                      Database.Get_User (Session.Get (SID, "LOGIN"))))));
         end if;
      end Value;

      LT : aliased Lazy_Tags;

   begin
      return Response.Build
        (MIME.Text_HTML,
         String'(Templates.Parse
           (Template_Filename,
              Translations,
              Lazy_Tag => LT'Unchecked_Access)));
   end Final_Parse;

   --------------------
   -- Forum_Callback --
   --------------------

   function Forum_Callback (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
      P   : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/forum/threads" then
         return Final_Parse
           (Request,
            Template_Defs.Forum_Threads.Template,
            Database.Get_Threads);

      elsif URI = "/forum/entry" then
         return Final_Parse
           (Request,
            Template_Defs.Forum_Entry.Template,
            Database.Get_Entry
              (Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.Id)));
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
      end if;

      return Response.Build
        (MIME.Text_XML,
         String'(Templates.Parse
           (Template_Defs.R_Block_Login.Template,
            (1 => Templates.Assoc (Template_Defs.R_Block_Login.Login,
             String'(Session.Get (SID, "LOGIN")))))));
   end Login_Callback;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/forum",
         Action => Dispatchers.Callback.Create (Forum_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/login_form_enter",
         Action => Dispatchers.Callback.Create (Login_Callback'Access));

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/we_js",
         Action => Dispatchers.Callback.Create (WEJS_Callback'Access),
         Prefix => True);

      Server.Log.Start (HTTP);

      Server.Log.Start_Error (HTTP);

      Config.Set.Session (Configuration, True);

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
