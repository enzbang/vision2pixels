------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2012                          --
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

with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.Containers.Tables;
with AWS.Dispatchers.Callback;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Context;
with AWS.Services.Web_Block.Registry;
with AWS.Session;
with AWS.Status;
with AWS.Templates;
with AWS.Utils;

with Gwiad.Plugins.Websites.Registry;
with Gwiad.Web.Virtual_Host;

with Morzhol.Logs;
with Morzhol.OS;

with V2P.Cache;
with V2P.Callbacks.Page;
with V2P.Callbacks.Ajax;
with V2P.Callbacks.Web_Block;
with V2P.Context;
with V2P.Database.Remember;
with V2P.Settings;
with V2P.URL;
with V2P.Version;

with V2P.Template_Defs.Block_Cdc;
with V2P.Template_Defs.Block_Forum_Category_Filter;
with V2P.Template_Defs.Block_Forum_Category_Set;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_Forum_Filter_Page_Size;
with V2P.Template_Defs.Block_Forum_Sort;
with V2P.Template_Defs.Block_Forum_Threads;
with V2P.Template_Defs.Block_Forum_Threads_Text;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_New_Theme;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_Pref_Css_Url;
with V2P.Template_Defs.Block_Pref_Forum_Filter;
with V2P.Template_Defs.Block_Pref_Forum_Filter_Page_Size;
with V2P.Template_Defs.Block_Pref_Forum_Sort;
with V2P.Template_Defs.Block_Pref_Image_Size;
with V2P.Template_Defs.Block_Pref_New_Avatar;
with V2P.Template_Defs.Block_Pref_Private_Message;
with V2P.Template_Defs.Block_Pref_Show_Comments;
with V2P.Template_Defs.Block_Pref_User_Email;
with V2P.Template_Defs.Block_Private_Message;
with V2P.Template_Defs.Block_Theme_Admin;
with V2P.Template_Defs.Block_Theme_Photos;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_User_Photo_List;
with V2P.Template_Defs.Block_Users;
with V2P.Template_Defs.Block_User_Avatar;
with V2P.Template_Defs.Block_Users_To_Validate;
with V2P.Template_Defs.Block_Vote_Week_Photo;

with V2P.Template_Defs.Chunk_Forum_List_Select;
with V2P.Template_Defs.Chunk_New_Comment_Photo;
with V2P.Template_Defs.Chunk_Users;
with V2P.Template_Defs.Chunk_V2p_Top;

with V2P.Template_Defs.Page_Admin;
with V2P.Template_Defs.Page_Admin_Database_Cleanup;
with V2P.Template_Defs.Page_Cdc;
with V2P.Template_Defs.Page_Delete_User;
with V2P.Template_Defs.Page_Error;
with V2P.Template_Defs.Page_Fatal_Error;
with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Page_Forum_New_Text_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Google_Map_View;
with V2P.Template_Defs.Page_Help;
with V2P.Template_Defs.Page_Lost_Password;
with V2P.Template_Defs.Page_Main;
with V2P.Template_Defs.Page_New;
with V2P.Template_Defs.Page_Photo_Post;
with V2P.Template_Defs.Page_Search;
with V2P.Template_Defs.Page_Termsofuse;
with V2P.Template_Defs.Page_User;
with V2P.Template_Defs.Page_User_Register;
with V2P.Template_Defs.Page_Users;
with V2P.Template_Defs.Page_Validate_New_Email;
with V2P.Template_Defs.Page_Validate_User;
with V2P.Template_Defs.Page_Week_Votes;
with V2P.Template_Defs.Page_Rss;
with V2P.Template_Defs.Page_Rss_Last_Comments;
with V2P.Template_Defs.Page_Rss_Last_Photos;
with V2P.Template_Defs.Page_Rss_Last_Posts;
with V2P.Template_Defs.Page_Theme;
with V2P.Template_Defs.Page_Themes;

with V2P.Template_Defs.Set_Global;

with V2P.Template_Defs.R_Block_Cdc;
with V2P.Template_Defs.R_Block_Comment_Form_Enter;
with V2P.Template_Defs.R_Block_Delete_User;
with V2P.Template_Defs.R_Block_Fatal_Error;
with V2P.Template_Defs.R_Block_Forum_Category_Set;
with V2P.Template_Defs.R_Block_Forum_Filter;
with V2P.Template_Defs.R_Block_Forum_List;
with V2P.Template_Defs.R_Block_Hidden_Status;
with V2P.Template_Defs.R_Block_Login;
with V2P.Template_Defs.R_Block_Logout;
with V2P.Template_Defs.R_Block_New_Theme;
with V2P.Template_Defs.R_Block_Metadata_Form_Enter;
with V2P.Template_Defs.R_Block_Post_Form_Enter;
with V2P.Template_Defs.R_Block_Pref_Private_Message;
with V2P.Template_Defs.R_Block_Pref_Show_Comments;
with V2P.Template_Defs.R_Block_Rate;
with V2P.Template_Defs.R_Block_Send_Private_Message;
with V2P.Template_Defs.R_Block_Theme_Admin;
with V2P.Template_Defs.R_Block_Theme_Photos;
with V2P.Template_Defs.R_Block_User_Email_Form_Enter;
with V2P.Template_Defs.R_Block_User_Page_Edit_Form_Enter;
with V2P.Template_Defs.R_Block_User_Photo_List;
with V2P.Template_Defs.R_Block_User_Preferences;
with V2P.Template_Defs.R_Block_Users;
with V2P.Template_Defs.R_Block_Users_To_Validate;
with V2P.Template_Defs.R_Block_Users_To_Validate_Message;
with V2P.Template_Defs.R_Block_Vote_Week_Photo;
with V2P.Template_Defs.R_Context_Error;
with V2P.Template_Defs.R_Page_Search;
with V2P.Template_Defs.R_Page_User_Register;
with V2P.Template_Defs.R_Page_Lost_Password;

package body V2P.Web_Server is

   use Ada;
   use Ada.Calendar;
   use Ada.Exceptions;
   use AWS;

   use Morzhol;
   use Morzhol.OS;

   use AWS.Services.Web_Block.Registry;
   use Gwiad.Plugins.Websites;

   use type Ada.Directories.File_Kind;

   Module          : constant Logs.Module_Name := "V2P.Web_Server";
   XML_Path        : constant String :=
                       Directories.Compose
                         (Containing_Directory => Gwiad_Plugin_Path,
                          Name                 => "xml");
   XML_Prefix_URI  : constant String := "/xml_";
   CSS_URI         : constant String := "/css";
   IMG_URI         : constant String := "/css/img";
   Web_JS_URI      : constant String := "/we_js";

   Timezone_Cookie : constant String := "V2PTZ";

   V2p_Lib_Path    : constant String :=
                       Gwiad.Plugins.Get_Last_Library_Path;

   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   In_Ten_Year : Calendar.Time;

   -------------------------
   --  Standard Callbacks --
   -------------------------

   function CSS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element CSS callback

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function Default_XML_Callback
     (Request : in Status.Data) return String;
   --  Default callback for xml action

   function Float_Mult_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String;
   --  Mult filter (template parser user filter)

   function URL_Encode_Internal
     (Value           : in String;
      Underline_Space : in Boolean) return String;
   --  Converts characters into a format that can be safely transmitted over
   --  the Internet
   --  If Underline_Space is True, replace space characters by an underscore.
   --  If False, encode spaces with %20.
   --  Please note that using Underline_Space creates a different URL.

   function URL_Encode_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String;
   --  Clean-up URL to be properly printed in plain text e-mail or used as link
   --  in Web pages. The most important part is to convert a space to %20 to
   --  avoid breaks into the URL.

   function URL_Encode_Name_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String;

   function IMG_Callback (Request : in Status.Data) return Response.Data;
   --  Image callback

   function Photos_Callback (Request : in Status.Data) return Response.Data;
   --  Photos callback

   function Website_Data (Request : in Status.Data) return Response.Data;
   --  Website data (images, ...) callback

   function WEJS_Callback (Request : in Status.Data) return Response.Data;
   --  Web Element JavaScript callback

   -------------
   --  Gwiad  --
   -------------

   procedure Unregister (Name : in Website_Name);
   --  Unregister website

   ------------------
   -- CSS_Callback --
   ------------------

   function CSS_Callback (Request : in Status.Data) return Response.Data is
      SID          : constant Session.Id := Status.Session (Request);
      URI          : constant String := Status.URI (Request);
      File         : constant String :=
                      Gwiad_Plugin_Path & Directory_Separator
                         & URI (URI'First + 1 .. URI'Last);
      C_File       : constant String := Cache.Name (File);
      Translations : Templates.Translate_Set;
   begin
      if not Directories.Exists (File)
        or else Directories.Kind (File) /= Directories.Ordinary_File
      then
         Does_Not_Exist : declare
            Translations : Templates.Translate_Set;
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body => String'(Templates.Parse
                                         (Template_Defs.Page_Error.Template,
                                          Translations)));
         end Does_Not_Exist;
      end if;

      if not Directories.Exists (C_File) then
         Templates.Insert
           (Translations,
            Templates.Assoc (Template_Defs.Set_Global.LOGIN,
              String'(Session.Get (SID, Template_Defs.Set_Global.LOGIN))));

         Cache.Create
           (File, Templates.Parse (File, Translations), Settings.Compression);
      end if;

      if Settings.Compression
        and then Status.Is_Supported (Request, Encoding => Messages.GZip)
      then
         return Response.File
           (MIME.Content_Type (File), Cache.Name_Compressed (File),
            Encoding => Messages.GZip);
      else
         return Response.File (MIME.Content_Type (File), C_File);
      end if;
   end CSS_Callback;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      use type Messages.Status_Code;

      procedure Read_Timezone;
      --  Read timezone as recorded in the cookies, set the session
      --  accordingly.

      SID : constant Session.Id := Status.Session (Request);

      -------------------
      -- Read_Timezone --
      -------------------

      procedure Read_Timezone is

         HH : constant Calendar.Formatting.Hour_Number :=
                Calendar.Formatting.Hour (Calendar.Clock);
         --  Timestamp changing obviously every hour, this is used to update
         --  the TZ session and context every hour. This way the time will get
         --  adjusted in case of DST.

         procedure Set_Timezone (TZ : in String);
         --  Set time in session

         ------------------
         -- Set_Timezone --
         ------------------

         procedure Set_Timezone (TZ : in String) is
            --  We check for '+' or '-' as starting character to avoid boggus
            --  timezone data.
         begin
            if TZ'Length = 0 then
               --  Protect against empty cookie sent
               return;

            elsif TZ (TZ'First) = '+' then
               Session.Set
                 (SID, Template_Defs.Set_Global.TZ,
                  '-' & TZ (TZ'First + 1 .. TZ'Last));
               Session.Set (SID, Template_Defs.Set_Global.TZHH, HH);

            elsif TZ (TZ'First) = '-' then
               Session.Set
                 (SID, Template_Defs.Set_Global.TZ,
                  '+' & TZ (TZ'First + 1 .. TZ'Last));
               Session.Set (SID, Template_Defs.Set_Global.TZHH, HH);
            end if;
         end Set_Timezone;

      begin
         if not Session.Exist (SID, Template_Defs.Set_Global.TZ)
           or else Session.Get (SID, Template_Defs.Set_Global.TZHH) /= HH
         then
            declare
               use Strings.Unbounded;

               Headers : constant AWS.Headers.List := Status.Header (Request);
               Cookies : constant Containers.Tables.VString_Array :=
                           Headers.Get_Values (AWS.Messages.Cookie_Token);
               Start   : Natural;
               Last    : Natural;
            begin
               for K in Cookies'Range loop
                  Start := Index (Cookies (K), Timezone_Cookie);
                  if Start /= 0 then
                     Last := Index (Cookies (K), ";", From => Start);

                     if Last = 0 then
                        Last := Length (Cookies (K));
                     else
                        Last := Last - 1;
                     end if;

                     Set_Timezone (Slice (Cookies (K), Start + 6, Last));
                  end if;
               end loop;
            end;
         end if;
      end Read_Timezone;

      URI          : constant String := Status.URI (Request);
      Headers      : constant AWS.Headers.List := AWS.Status.Header (Request);
      Cookie       : constant String :=
                       AWS.Headers.Get_Values
                         (Headers, AWS.Messages.Cookie_Token);
      Context      : aliased Services.Web_Block.Context.Object :=
                       Services.Web_Block.Registry.Get_Context
                         (Request => Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin
      Read_Timezone;

      --  Update the context

      V2P.Context.Update (Context'Access, SID, Cookie);

      --  Add LOGIN and ADMIN in template

      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Set_Global.LOGIN,
               String'(Session.Get (SID, Template_Defs.Set_Global.LOGIN))));

         if Session.Exist (SID, Template_Defs.Set_Global.ADMIN) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.Set_Global.ADMIN,
                  String'(Session.Get
                    (SID, Template_Defs.Set_Global.ADMIN))));
         end if;
      end if;

      --  Add Version number

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.V2P_VERSION,
            V2P.Version.Simple));

      --  Add Google Map key

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Chunk_V2p_Top.GOOGLE_MAP_KEY,
            Settings.Google_Map_Key));

      --  Add some URL

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_THREAD_URL,
            Template_Defs.Page_Forum_Threads.Set.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_THREAD_URL_PREFIX,
            Template_Defs.Page_Forum_Threads.Set.URL_PREFIX));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_POST_URL,
            Template_Defs.Page_Forum_New_Text_Entry.Set.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_NEW_PHOTO_URL,
            Template_Defs.Page_Photo_Post.Set.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.Set.URL));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL_PREFIX,
            Template_Defs.Page_Forum_Entry.Set.URL_PREFIX));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL_CDC_PREFIX,
            Template_Defs.Page_Forum_Entry.Set.URL_CDC_PREFIX));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.ADMIN_URL,
            Template_Defs.Page_Admin.Set.URL));

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
            Settings.Big_Images_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.MEDIUM_IMAGE_SOURCE_PREFIX,
            Settings.Medium_Images_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.AVATAR_SOURCE_PREFIX,
            Settings.Avatars_Source_Prefix));

      V2P.Callbacks.Web_Block.Pref_Image_Size
        (Request, Context'Access, Translations);

      V2P.Callbacks.Web_Block.Pref_Show_Comments
        (Request, Context'Access, Translations);

      V2P.Callbacks.Web_Block.Pref_CSS_URL
        (Request, Context'Access, Translations);

      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN)
         and then Context.Exist (Template_Defs.Set_Global.LAST_VISIT_TOKEN)
      then
         Context.Remove (Template_Defs.Set_Global.LAST_VISIT_TOKEN);
         --  Do not update last visit again !
      end if;

      if Services.Web_Block.Registry.Content_Type (URI) = MIME.Text_HTML then
         --  The HTML case, we just redirect to the Web root
         Web_Page := Services.Web_Block.Registry.Build
           (URI, Request, Translations,
            Cache_Control => Messages.Prevent_Cache,
            Context       => Context'Access,
            Context_Error => "/");
      else
         Web_Page := Services.Web_Block.Registry.Build
           (URI, Request, Translations,
            Cache_Control => Messages.Prevent_Cache,
            Context       => Context'Access,
            Context_Error =>
              Template_Defs.R_Context_Error.Set.CONTEXT_ERROR_URL);
      end if;

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         Web_Page := Services.Web_Block.Registry.Build
           (Template_Defs.Page_Error.Set.URL, Request, Translations);
      end if;

      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN)
        and then not Context.Exist ("cookie")
      then
         Set_Cookie : declare
            use type Ada.Calendar.Arithmetic.Day_Count;
            Valid_Days : constant Ada.Calendar.Arithmetic.Day_Count := 15;
            HTTP_Date  : constant String :=
                           AWS.Messages.To_HTTP_Date
                             (Ada.Calendar.Clock + Valid_Days);
            GenCookie  : constant String :=
                           Database.Remember.Gen_Cookie
                             (Session.Get
                                (SID, Template_Defs.Set_Global.LOGIN));
         begin
            AWS.Response.Set.Add_Header
              (Web_Page, AWS.Messages.Set_Cookie_Token,
               Value => "v2p=" & GenCookie
               & "; expires=" & HTTP_Date & "; path=/");
            Context.Set_Value ("cookie", "set");
         end Set_Cookie;
      end if;

      if Session.Exist (SID, Template_Defs.Set_Global.LOGIN)
         and then Context.Exist (Template_Defs.Set_Global.LAST_VISIT_TOKEN)
      then
         --  Update last visit table

         Database.Set_Last_Visit
           (Session.Get (SID, Template_Defs.Set_Global.LOGIN),
            V2P.Context.Not_Null_Counter.Get_Value
              (Context, Template_Defs.Set_Global.LAST_VISIT_TOKEN));
      end if;

      return Web_Page;

   exception
      when Callbacks.Page.Error_404 =>
         --  Page not found
         Web_Page := Services.Web_Block.Registry.Build
           (Template_Defs.Page_Error.Set.URL, Request, Translations);
         return Web_Page;

      when E : others =>
         Fatal_Error : begin
            if
              Services.Web_Block.Registry.Content_Type (URI) = MIME.Text_HTML
            then
               Logs.Write
                 (Name    => Module,
                  Kind    => Logs.Error,
                  Content => "Default_Callback HTML exception for "
                  & Logs.NV ("URI", URI) & " "
                  & Logs.NV ("EXNAME", Exception_Name (E)) & " "
                  & Logs.NV ("EXMESS", Exception_Message (E)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Page_Fatal_Error.EXCEPTION_MSG,
                     "Default_Callback HTML exception for "
                     & Logs.NV ("URI", URI) & " "
                     & Logs.NV ("EXNAME", Exception_Name (E)) & " "
                     & Logs.NV ("EXMESS", Exception_Message (E))));

               return Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body => String'(Templates.Parse
                    (Template_Defs.Page_Fatal_Error.Template,
                       Translations)));

            else
               Logs.Write
                 (Name    => Module,
                  Kind    => Logs.Error,
                  Content => "Default_Callback XML exception for "
                  & Logs.NV ("URI", URI) & " "
                  & Logs.NV ("EXNAME", Exception_Name (E)) & " "
                  & Logs.NV ("EXMESS", Exception_Message (E)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.R_Block_Fatal_Error.EXCEPTION_MSG,
                     "Default_Callback XML exception for "
                     & Logs.NV ("URI", URI) & " "
                     & Logs.NV ("EXNAME", Exception_Name (E)) & " "
                     & Logs.NV ("EXMESS", Exception_Message (E))));

               return Response.Build
                 (Content_Type => MIME.Text_XML,
                  Message_Body => String'(Templates.Parse
                    (Template_Defs.R_Block_Fatal_Error.Template,
                       Translations)));
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

   -----------------------
   -- Float_Mult_Filter --
   -----------------------

   function Float_Mult_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String
   is
      pragma Unreferenced (Context);
      N, V : Float;
      R    : String (1 .. 1000);
   begin
      N := Float'Value (Parameters);
      V := Float'Value (Value);

      Float_Text_IO.Put (To => R, Item => V * N, Aft => 2, Exp => 0);

      return R;
   end Float_Mult_Filter;

   ------------------
   -- IMG_Callback --
   ------------------

   function IMG_Callback (Request : in Status.Data) return Response.Data is
      URI    : constant String := Status.URI (Request);
      File   : constant String :=
                 Gwiad_Plugin_Path & Directory_Separator
                   & URI (URI'First + 1 .. URI'Last);
      Result : AWS.Response.Data;
   begin
      if not Directories.Exists (File)
        or else Directories.Kind (File) /= Directories.Ordinary_File
      then
         Does_Not_Exist : declare
            Translations : Templates.Translate_Set;
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body => String'(Templates.Parse
                                         (Template_Defs.Page_Error.Template,
                                          Translations)));
         end Does_Not_Exist;
      end if;

      Result := Response.File (MIME.Content_Type (File), File);

      AWS.Response.Set.Add_Header
        (Result,
         AWS.Messages.Expires_Token,
         AWS.Messages.To_HTTP_Date (In_Ten_Year));

      return Result;
   end IMG_Callback;

   ---------------------
   -- Photos_Callback --
   ---------------------

   function Photos_Callback (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);

      function Get_Filename return String;
      --  Returns image filename

      --------------
      -- Filename --
      --------------

      function Get_Filename return String is

         function URI_Prefix return String;
         --  Returns the prefix for the current URI, this is the string before
         --  the second /.

         ----------------
         -- URI_Prefix --
         ----------------

         function URI_Prefix return String is
            K : constant Natural :=
                  Strings.Fixed.Index (URI, "/", From => URI'First + 1);
         begin
            return URI (URI'First .. K - 1);
         end URI_Prefix;

         Medium : constant String :=
                    Compose
                      (V2P.URL.Medium_Images_Full_Prefix,
                       URI (URI'First +
                           Settings.Medium_Images_Source_Prefix'Length + 1
                         .. URI'Last));

         Prefix : constant String := URI_Prefix;

      begin
         --  For compatibility with older version, if the medium sized image
         --  does not exist we return the full one. This was indeed a photo
         --  with medium size anyway.

         if Prefix = Settings.Big_Images_Source_Prefix
           or else (Prefix = Settings.Medium_Images_Source_Prefix and then
                      not Directories.Exists (Medium))
         then
            return Compose
              (V2P.URL.Big_Images_Full_Prefix,
               URI (URI'First + Prefix'Length + 1 .. URI'Last));

         elsif Prefix = Settings.Medium_Images_Source_Prefix then
            return Medium;

         elsif Prefix = Settings.Thumbs_Source_Prefix then
            return Compose
              (V2P.URL.Thumbs_Full_Prefix,
               URI (URI'First + Prefix'Length + 1 .. URI'Last));

         else
            return Compose
              (V2P.URL.Avatar_Full_Prefix,
               URI (URI'First + Prefix'Length + 1 .. URI'Last));
         end if;

      exception
         when others =>
            Logs.Write
              (Name => Module, Kind => Logs.Error, Content => "Exception");
            raise;
      end Get_Filename;

      Filename : constant String := Get_Filename;
      Result   : AWS.Response.Data;
   begin
      if not Directories.Exists (Filename)
        or else Directories.Kind (Filename) /= Directories.Ordinary_File
      then
         Does_Not_Exist : declare
            Translations : Templates.Translate_Set;
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body => String'(Templates.Parse
                                         (Template_Defs.Page_Error.Template,
                                          Translations)));
         end Does_Not_Exist;

      else
         Result := Response.File (MIME.Content_Type (Filename), Filename);

         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Expires_Token,
            AWS.Messages.To_HTTP_Date (In_Ten_Year));

         return Result;
      end if;
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
         IMG_URI,
         Action => Dispatchers.Callback.Create (IMG_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         CSS_URI,
         Action => Dispatchers.Callback.Create (CSS_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Big_Images_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Medium_Images_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Thumbs_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
         Prefix => True);

      Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         Settings.Avatars_Source_Prefix,
         Action => Dispatchers.Callback.Create (Photos_Callback'Access),
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
        (Key      => Template_Defs.Page_User.Set.URL,
         Template => Template_Defs.Page_User.Template,
         Data_CB  => Callbacks.Page.User'Access,
         Prefix   => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Entry.Set.URL,
         Template_Defs.Page_Forum_Entry.Template,
         Callbacks.Page.Forum_Entry'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Forum_Entry.Set.URL_PHOTOGRAPHY_PREFIX,
         Regexp   => "([0-9]+)-.*",
         Template => Template_Defs.Page_Forum_Entry.Template,
         Data_CB  => Callbacks.Page.Forum_Entry_P'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Forum_Entry.Set.URL_CDC_PREFIX,
         Regexp   => "([0-9]+)-.*",
         Template => Template_Defs.Page_Forum_Entry.Template,
         Data_CB  => Callbacks.Page.Forum_Entry_CdC_P'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Forum_Entry.Set.URL_PREFIX,
         Regexp   => "([0-9]+)-.*",
         Template => Template_Defs.Page_Forum_Entry.Template,
         Data_CB  => Callbacks.Page.Forum_Entry_P'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Theme.Set.URL_PREFIX,
         Regexp   => "([0-9]+).*",
         Template => Template_Defs.Page_Theme.Template,
         Data_CB  => Callbacks.Page.Theme'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Threads.Set.URL,
         Template_Defs.Page_Forum_Threads.Template,
         Callbacks.Page.Forum_Threads'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Forum_Threads.Set.URL_PREFIX,
         Regexp   => "([0-9]+)-.*",
         Template => Template_Defs.Page_Forum_Threads.Template,
         Data_CB  => Callbacks.Page.Forum_Threads_P'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Main.Set.URL,
         Template_Defs.Page_Main.Template,
         Callbacks.Page.Main'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Users.Set.URL,
         Template_Defs.Page_Users.Template,
         Callbacks.Page.Users'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Error.Set.URL,
         Template_Defs.Page_Error.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Admin.Set.URL,
         Template_Defs.Page_Admin.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Photo_Post.Set.URL,
         Template_Defs.Page_Photo_Post.Template,
         Callbacks.Page.Post_Photo'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Text_Entry.Set.URL,
         Template_Defs.Page_Forum_New_Text_Entry.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Photo_Entry.Set.URL,
         Template_Defs.Page_Forum_New_Photo_Entry.Template,
         Callbacks.Page.New_Photo_Entry'Access,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Themes.Set.URL,
         Template_Defs.Page_Themes.Template,
         Callbacks.Page.Themes'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Set.FORM_POST_COMMENT_PHOTO,
         Template_Defs.Chunk_New_Comment_Photo.Template,
         Callbacks.Page.New_Photo_Entry'Access,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_New_Avatar.Set.FORM_NEW_AVATAR,
         Template_Defs.Block_User_Avatar.Template,
         Callbacks.Page.New_Avatar'Access,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Avatar.Set.URL,
         Template_Defs.Block_User_Avatar.Template,
         Callbacks.Web_Block.User_Avatar'Access);
      --  To get the avatar from the iframe in the user's page

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Validate_User.Set.URL,
         Template_Defs.Page_Validate_User.Template,
         Callbacks.Page.Validate_User'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Delete_User.Set.URL,
         Template_Defs.Page_Delete_User.Template,
         Callbacks.Page.Delete_User'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Cdc.Set.URL_PREFIX,
         Regexp   => "/?([a-z]*).*",
         Template => Template_Defs.Page_Cdc.Template,
         Data_CB  => Callbacks.Page.CdC'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Search.Set.URL,
         Template_Defs.Page_Search.Template,
         Callbacks.Page.Search'Access);

      Services.Web_Block.Registry.Register_Pattern_URL
        (Prefix   => Template_Defs.Page_Week_Votes.Set.URL_PREFIX,
         Regexp   => "([0-9]+).*",
         Template => Template_Defs.Page_Week_Votes.Template,
         Data_CB  => Callbacks.Page.Week_Votes'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Help.Set.URL,
         Template_Defs.Page_Help.Template,
         null);
      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Help.Set.URL_ALIAS,
         Template_Defs.Page_Help.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_User_Register.Set.URL,
         Template_Defs.Page_User_Register.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Lost_Password.Set.URL,
         Template_Defs.Page_Lost_Password.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_New.Set.URL,
         Template_Defs.Page_New.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Termsofuse.Set.URL,
         Template_Defs.Page_Termsofuse.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Google_Map_View.Set.URL,
         Template_Defs.Page_Google_Map_View.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Validate_New_Email.Set.URL,
         Template_Defs.Page_Validate_New_Email.Template,
         Callbacks.Page.Validate_New_Email'Access);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Admin_Database_Cleanup.Set.URL,
         Template_Defs.Page_Admin_Database_Cleanup.Template,
         Callbacks.Page.Admin_Database_Cleanup'Access);

      --  Register RSS

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Rss.Set.URL,
         Template_Defs.Page_Rss.Template,
         null);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Rss_Last_Comments.Set.URL,
         Template_Defs.Page_Rss_Last_Comments.Template,
         Callbacks.Page.Rss_Last_Comments'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => False);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Rss_Last_Posts.Set.URL,
         Template_Defs.Page_Rss_Last_Posts.Template,
         Callbacks.Page.Rss_Last_Posts'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => False);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Rss_Last_Photos.Set.URL,
         Template_Defs.Page_Rss_Last_Photos.Template,
         Callbacks.Page.Rss_Last_Photos'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => False);

      --  Register Ajax callbacks

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_bl_login_form_enter,
         Template_Defs.R_Block_Login.Template,
         Callbacks.Ajax.Login'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Login.Ajax.onclick_bl_logout_enter,
         Template_Defs.R_Block_Logout.Template,
         Callbacks.Ajax.Logout'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onchange_Filter_Forum'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Forum_Filter.Ajax.
           onchange_bpff_forum_filter_set,
         Template_Defs.R_Block_User_Preferences.Template,
         Callbacks.Ajax.Onchange_Filter_Forum_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Forum_Filter_Page_Size.Ajax.
           onchange_bpffps_forum_filter_pagesize_set,
         Template_Defs.R_Block_User_Preferences.Template,
         Callbacks.Ajax.Onchange_Filter_Forum_Page_Size_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Forum_Sort.Ajax.onchange_bpfs_forum_sort_set,
         Template_Defs.R_Block_User_Preferences.Template,
         Callbacks.Ajax.Onchange_Filter_Sort_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Css_Url.Ajax.onclick_bpcu_css_url_form_enter,
         Template_Defs.R_Block_User_Preferences.Template,
         Callbacks.Ajax.Onclick_CSS_URL_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Private_Message.Ajax.onclick_bppm_check,
         Template_Defs.R_Block_Pref_Private_Message.Template,
         Callbacks.Ajax.Onclick_Pref_Private_Message_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Show_Comments.Ajax.onclick_bpsc_check,
         Template_Defs.R_Block_Pref_Show_Comments.Template,
         Callbacks.Ajax.Onclick_Pref_Show_Comments'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Category_Filter.
           Ajax.onchange_bfcf_forum_category_filter_set,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onchange_Category_Filter_Forum'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_Image_Size.Ajax.
           onchange_bpis_image_size,
         Template_Defs.R_Block_User_Preferences.Template,
         Callbacks.Ajax.Onchange_Image_Size_Preference'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Cdc.Ajax.onclick_bcdc_goto_next_page,
         Template_Defs.R_Block_Cdc.Template,
         Callbacks.Ajax.Onclick_CdC_Goto_Next_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Cdc.Ajax.onclick_bcdc_goto_previous_page,
         Template_Defs.R_Block_Cdc.Template,
         Callbacks.Ajax.Onclick_CdC_Goto_Previous_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Threads.Ajax.onclick_bft_goto_next_page,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onclick_Goto_Next_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Threads.Ajax.onclick_bft_goto_previous_page,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onclick_Goto_Previous_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users.Ajax.onclick_bu_goto_next_page,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Goto_Next_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users.Ajax.onclick_bu_goto_previous_page,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Goto_Previous_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Users.Ajax.onclick_cu_registered_on,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Sort_Registered_On'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Photo_List.Ajax.onclick_bupl_goto_next_page,
         Template_Defs.R_Block_User_Photo_List.Template,
         Callbacks.Ajax.Onclick_User_Photo_List_Goto_Next_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Users.Ajax.onclick_cu_last_connected,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Sort_Last_Connected'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Users.Ajax.onclick_cu_nb_photos,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Sort_Nb_Photos'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Users.Ajax.onclick_cu_nb_comments,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Sort_Nb_Comments'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Users.Ajax.onclick_cu_nb_cdcs,
         Template_Defs.R_Block_Users.Template,
         Callbacks.Ajax.Onclick_Users_Sort_Nb_CdC'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Photo_List.
           Ajax.onclick_bupl_goto_previous_page,
         Template_Defs.R_Block_User_Photo_List.Template,
         Callbacks.Ajax.Onclick_User_Photo_List_Goto_Previous_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Threads_Text.
           Ajax.onclick_bftt_goto_next_page,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onclick_Goto_Next_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Threads_Text.
           Ajax.onclick_bftt_goto_previous_page,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onclick_Goto_Previous_Page'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Filter_Page_Size.
           Ajax.onchange_bffps_forum_filter_pagesize,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onchange_Filter_Forum_Page_Size'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Sort.Ajax.onchange_bfs_forum_sort_set,
         Template_Defs.R_Block_Forum_Filter.Template,
         Callbacks.Ajax.Onchange_Forum_Sort'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Forum_Category_Set.
           Ajax.onchange_bfcs_forum_category_set,
         Template_Defs.R_Block_Forum_Category_Set.Template,
         Callbacks.Ajax.Onchange_Category_Set'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_Entry.Ajax.onclick_pfe_hidden_status_toggle,
         Template_Defs.R_Block_Hidden_Status.Template,
         Callbacks.Ajax.Onclick_Hidden_Status_Toggle'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Chunk_Forum_List_Select.
           Ajax.onchange_cfls_sel_forum_list,
         Template_Defs.R_Block_Forum_List.Template,
         Callbacks.Ajax.Onchange_Forum_List'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Private_Message.Ajax.onsubmit_bpm_form,
         Template_Defs.R_Block_Send_Private_Message.Template,
         Callbacks.Ajax.Onsubmit_Private_Message'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onsubmit_bnc_comment_form,
         Template_Defs.R_Block_Comment_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Comment_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Comment.Ajax.onsubmit_bnc_comment_register,
         Template_Defs.R_Block_Comment_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Comment_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Text_Entry.
           Ajax.onsubmit_pfnte_new_entry_form_submit,
         Template_Defs.R_Block_Post_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Post_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Forum_New_Photo_Entry.
           Ajax.onsubmit_pfnpe_new_entry_form_submit,
         Template_Defs.R_Block_Post_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Post_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Metadata.Ajax.onsubmit_bm_metadata_post,
         Template_Defs.R_Block_Metadata_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_Metadata_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_User_Page.Ajax.onsubmit_bup_user_page_edit_form,
         Template_Defs.R_Block_User_Page_Edit_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_User_Page_Edit_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Vote.Set.AJAX_RATE_URL,
         Template_Defs.R_Block_Rate.Template,
         Callbacks.Ajax.Onsubmit_Rate'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Vote_Week_Photo.Ajax.onclick_bvwp_vote_button,
         Template_Defs.R_Block_Vote_Week_Photo.Template,
         Callbacks.Ajax.Onclick_Vote_Week_Photo'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_User_Register.Ajax.onsubmit_pur_register_user,
         Template_Defs.R_Page_User_Register.Template,
         Callbacks.Ajax.Onsubmit_Pur_Register_User'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Lost_Password.Ajax.onsubmit_plp_lost_password,
         Template_Defs.R_Page_Lost_Password.Template,
         Callbacks.Ajax.Onsubmit_Plp_Lost_Password'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users_To_Validate.
           Ajax.onclick_butv_send_reminders,
         Template_Defs.R_Block_Users_To_Validate.Template,
         Callbacks.Ajax.Onclick_Send_Reminders'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users_To_Validate.
           Ajax.onclick_butv_send_reminder,
         Template_Defs.R_Block_Users_To_Validate.Template,
         Callbacks.Ajax.Onclick_Send_Reminders'Access,
         Prefix           => True,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users_To_Validate.
           Ajax.onclick_butv_show_reminder,
         Template_Defs.R_Block_Users_To_Validate_Message.Template,
         Callbacks.Ajax.Onclick_Show_Reminder'Access,
         Prefix           => True,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Users_To_Validate.
           Ajax.onclick_butv_del_user,
         Template_Defs.R_Block_Delete_User.Template,
         Callbacks.Ajax.Onclick_Delete_User'Access,
         Prefix           => True,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Page_Search.Ajax.onsubmit_ps_search_form,
         Template_Defs.R_Page_Search.Template,
         Callbacks.Ajax.Onsubmit_Search_Form'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Pref_User_Email.Ajax.onsubmit_bue_form,
         Template_Defs.R_Block_User_Email_Form_Enter.Template,
         Callbacks.Ajax.Onsubmit_User_Email_Form_Enter'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_New_Theme.Ajax.onsubmit_bnt_theme_form,
         Template_Defs.R_Block_New_Theme.Template,
         Callbacks.Ajax.Onsubmit_New_Theme'Access,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.R_Context_Error.Set.CONTEXT_ERROR_URL,
         Template_Defs.R_Context_Error.Template,
         Callbacks.Ajax.On_Context_Error'Access,
         Content_Type => MIME.Text_XML);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Theme_Photos.Ajax.onclick_btp_sel,
         Template_Defs.R_Block_Theme_Photos.Template,
         Callbacks.Ajax.Onclick_Theme_Vote'Access,
         Prefix           => True,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (Template_Defs.Block_Theme_Admin.Ajax.onclick_bta_next_stage,
         Template_Defs.R_Block_Theme_Admin.Template,
         Callbacks.Ajax.Onclick_Theme_Next_Stage'Access,
         Prefix           => True,
         Content_Type     => MIME.Text_XML,
         Context_Required => True);

      Services.Web_Block.Registry.Register
        (XML_Prefix_URI,
         Default_XML_Callback'Access,
         null,
         Content_Type => MIME.Text_XML);
      --  All URLs starting with XML_Prefix_URI are handled by a specific
      --  callback returning the corresponding file in the xml directory.
   end Register_Callbacks;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      pragma Unreferenced (Name);
   begin
      Gwiad.Web.Virtual_Host.Unregister (Settings.Virtual_Host);
   end Unregister;

   -----------------------
   -- URL_Encode_Filter --
   -----------------------

   function URL_Encode_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String
   is
      pragma Unreferenced (Parameters, Context);
   begin
      return URL_Encode_Internal (Value, False);
   end URL_Encode_Filter;

   -------------------------
   -- URL_Encode_Internal --
   -------------------------

   function URL_Encode_Internal
     (Value           : in String;
      Underline_Space : in Boolean) return String
   is
      Result : String (Value'First .. Value'Last + Value'Length * 2);
      J      : Natural := Result'First - 1;
   begin
      for K in Value'Range loop
         if Characters.Handling.Is_Alphanumeric (Value (K)) then
            J := J + 1;
            Result (J) := Value (K);

         else
            declare
               P : constant Natural := Character'Pos (Value (K));
            begin
               if Underline_Space and then Value (K) = ' ' then
                  J := J + 1;
                  Result (J) := '_';

               elsif P <= 128 then
                  if P = Character'Pos ('/') then
                     Result (J + 1) := '$';
                  else
                     Result (J + 1) := '%';
                  end if;

                  Result (J + 2 .. J + 3) := Utils.Hex (P, Width => 2);
                  J := J + 3;

               else
                  J := J + 1;
                  Result (J) := Value (K);
               end if;
            end;
         end if;
      end loop;

      return Result (Result'First .. J);
   end URL_Encode_Internal;

   ----------------------------
   -- URL_Encode_Name_Filter --
   ----------------------------

   function URL_Encode_Name_Filter
     (Value      : in String;
      Parameters : in String;
      Context    : in Templates.Filter_Context) return String
   is
      pragma Unreferenced (Parameters, Context);
   begin
      return URL_Encode_Internal (Value, True);
   end URL_Encode_Name_Filter;

   ------------------
   -- Website_Data --
   ------------------

   function Website_Data (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String := Morzhol.OS.Compose
        (Gwiad_Plugin_Path,
         Settings.Website_Data_Path & Directory_Separator
         & URI
           (URI'First + Settings.Website_Data_Prefix'Length + 1 .. URI'Last));
   begin
      if not Directories.Exists (File)
        or else Directories.Kind (File) /= Directories.Ordinary_File
      then
         Does_Not_Exist : declare
            Translations : Templates.Translate_Set;
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body => String'(Templates.Parse
                                         (Template_Defs.Page_Error.Template,
                                          Translations)));
         end Does_Not_Exist;
      end if;

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
      C_File       : constant String := Cache.Name (File);
      Translations : Templates.Translate_Set;
   begin
      if not Directories.Exists (File)
        or else Directories.Kind (File) /= Directories.Ordinary_File
      then
         Does_Not_Exist : declare
            Translations : Templates.Translate_Set;
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body => String'(Templates.Parse
                                         (Template_Defs.Page_Error.Template,
                                          Translations)));
         end Does_Not_Exist;
      end if;

      if not Directories.Exists (C_File) then
         Cache.Create
           (File, Templates.Parse (File, Translations), Settings.Compression);
      end if;

      if Settings.Compression
        and then Status.Is_Supported (Request, Encoding => Messages.GZip)
      then
         return Response.File
           (MIME.Content_Type (File), Cache.Name_Compressed (File),
            Encoding => Messages.GZip);
      else
         return Response.File (MIME.Content_Type (File), C_File);
      end if;
   end WEJS_Callback;

begin  -- V2P.Web_Server : register vision2pixels website
   --  First we want to be sure that the cached files from the previous session
   --  are removed.

   V2P.Template_Defs.Lazy.Register;

   Cache.Clear (Root_Directory => Settings.Cache_Path);

   Set_Log : declare
      Log_Dir : constant String := Morzhol.OS.Compose
        (Gwiad_Plugin_Path, Settings.Log_Path);
   begin
      if not Ada.Directories.Exists (Log_Dir) then
         Ada.Directories.Create_Directory (Log_Dir);
      end if;

      Morzhol.Logs.Set_File (Log_Dir & Directory_Separator & "v2p.log");
   end Set_Log;

   AWS.Templates.Register_Filter ("FLOATMULT", Float_Mult_Filter'Access);
   AWS.Templates.Register_Filter ("URL_ENCODE", URL_Encode_Filter'Access);
   AWS.Templates.Register_Filter
      ("URL_ENCODE_NAME", URL_Encode_Name_Filter'Access);

   Register_Callbacks;

   Gwiad.Web.Virtual_Host.Register
     (Hostname => Settings.Virtual_Host,
      Action   => Main_Dispatcher);

   Gwiad.Plugins.Websites.Registry.Register
     (Name         => "vision2pixels",
      Description  => "a Web space engine to comment user's photos",
      Unregister   => Unregister'Access,
      Library_Path => V2p_Lib_Path);

   --  Init Ten_Year_From_Now

   In_Ten_Year := Clock + (3_650.0 * 86_400.0);

end V2P.Web_Server;
