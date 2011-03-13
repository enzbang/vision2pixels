------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2011                          --
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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.Parameters;
with AWS.Utils;

with Image.Data;
with Morzhol.OS;

with V2P.Callbacks.Web_Block;
with V2P.Context;
with V2P.Database.Admin;
with V2P.Database.Preference;
with V2P.Database.Registration;
with V2P.Navigation_Links;
with V2P.Settings;
with V2P.URL;
with V2P.Utils;

with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Pref_New_Avatar;
with V2P.Template_Defs.Block_User_Avatar;
with V2P.Template_Defs.Chunk_Navlink;
with V2P.Template_Defs.Page_Admin_Database_Cleanup;
with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Page_Validate_User;
with V2P.Template_Defs.Page_Validate_New_Email;
with V2P.Template_Defs.Page_Photo_Post;
with V2P.Template_Defs.Page_Rss_Last_Comments;
with V2P.Template_Defs.Page_Rss_Last_Posts;
with V2P.Template_Defs.Page_User;
with V2P.Template_Defs.Set_Global;

package body V2P.Callbacks.Page is

   use Ada;

   procedure Forum_Entry_Internal
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      TID          : in              Database.Id;
      From_Main    : in              Boolean);

   procedure Forum_Threads_Internal
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      FID          : in              Database.Id;
      From         : in              Natural);

   ----------------------------
   -- Admin_Database_Cleanup --
   ----------------------------

   procedure Admin_Database_Cleanup
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context, Request);
   begin
      Database.Admin.Database_Cleanup;
      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Page_Admin_Database_Cleanup.RESPONSE, "OK"));
   exception
      when others =>
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Page_Admin_Database_Cleanup.RESPONSE, "NOK"));
   end Admin_Database_Cleanup;

   ---------
   -- CdC --
   ---------

   procedure CdC
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Context.Remove (Template_Defs.Set_Global.NAV_FROM);
   end CdC;

   -----------------
   -- Delete_User --
   -----------------

   procedure Delete_User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P     : constant Parameters.List := Status.Parameters (Request);
      Login : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.LOGIN);
      Key   : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.KEY);
   begin
      if not Database.Registration.Delete_User (Login, Key) then
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Page_Validate_User.ERROR, True));
      end if;
   end Delete_User;

   -----------------
   -- Forum_Entry --
   -----------------

   procedure Forum_Entry
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      P         : constant Parameters.List := Status.Parameters (Request);
      TID       : constant Database.Id :=
                    Database.Id'Value
                      (Parameters.Get
                         (P, Template_Defs.Page_Forum_Entry.HTTP.TID));
      From_Main : constant Boolean :=
                    Parameters.Exist
                      (P, Template_Defs.Page_Forum_Entry.HTTP.From_Main)
                        and then Boolean'Value
                          (Parameters.Get
                             (P,
                              Template_Defs.Page_Forum_Entry.HTTP.From_Main));
   begin
      Forum_Entry_Internal (Request, Context, Translations, TID, From_Main);
   end Forum_Entry;

   -----------------------
   -- Forum_Entry_Cdc_P --
   -----------------------

   procedure Forum_Entry_CdC_P
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Parameters   :                 Callback_Parameters;
      Translations : in out          Templates.Translate_Set)
   is
      TID : constant Database.Id :=
              Database.Id'Value
                (Strings.Unbounded.To_String (Parameters (1)));
   begin
      Forum_Entry_Internal (Request, Context, Translations, TID, True);
   end Forum_Entry_CdC_P;

   --------------------------
   -- Forum_Entry_Internal --
   --------------------------

   procedure Forum_Entry_Internal
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      TID          : in              Database.Id;
      From_Main    : in              Boolean)
   is
      Login       : constant String :=
                      Context.Get_Value (Template_Defs.Set_Global.LOGIN);
      Admin       : constant Boolean :=
                      Context.Exist (Template_Defs.Set_Global.ADMIN)
                    and then Context.Get_Value
                      (Template_Defs.Set_Global.ADMIN) = "TRUE";
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the context

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.TID,
         Value   => TID);

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.TID, Database.Id'Image (TID)));

      if TID /= Database.Empty_Id then
         if not Settings.Anonymous_Visit_Counter then
            --  Do not count anonymous click
            --  Do not count author click if Ignore_Author_Click

            Count_Visit := Login /= ""
              and not (Settings.Ignore_Author_Click
                       and then Database.Is_Author (Login, TID));
         end if;

         if Count_Visit then
            Database.Increment_Visit_Counter (TID);
         end if;

         if From_Main then
            --  This is a link from the latest photo list or the CdC
            --  Generate navigations links.

            --  Reset category filter as we do not know the category
            Context.Set_Value (Template_Defs.Set_Global.FILTER_CATEGORY, "");

            V2P.Context.Counter.Set_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID,
               Value   => Database.Get_Forum_Id (TID));

            V2P.Callbacks.Web_Block.Forum_Threads
              (Request, Context, Translations);
         end if;

         --  Insert navigation links (previous and next post)

         Insert_Links : declare
            Previous_Id : constant Database.Id :=
                            Navigation_Links.Previous_Post (Context, TID);
            Next_Id     : constant Database.Id :=
                            Navigation_Links.Next_Post (Context, TID);
         begin
            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Page_Forum_Entry.PREVIOUS, Previous_Id));

            if Previous_Id /= Database.Empty_Id then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Page_Forum_Entry.PREVIOUS_THUMB,
                     Database.Get_Thumbnail (Previous_Id)));
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Chunk_Navlink.PREVIOUS_NAME,
                     Database.Get_Forum_Entry_Name (Previous_Id)));
            end if;

            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Page_Forum_Entry.NEXT, Next_Id));

            if Next_Id /= Database.Empty_Id then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Page_Forum_Entry.NEXT_THUMB,
                     Database.Get_Thumbnail (Next_Id)));
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Chunk_Navlink.NEXT_NAME,
                     Database.Get_Forum_Entry_Name (Next_Id)));
            end if;
         end Insert_Links;

         --  Insert the entry information

         Templates.Insert
           (Translations,
            Database.Get_Post
              (Tid        => TID,
               Forum_Type => Database.Get_Forum_Type (TID),
               TZ         => Context.Get_Value (Template_Defs.Set_Global.TZ),
               Admin      => Admin));

         --  Update user 'last visit' data
         --  This should be done just before returning the Web_Page in
         --  Default_Callback, when all lazy tags have been parsed.

         V2P.Context.Not_Null_Counter.Set_Value
           (Context.all, Template_Defs.Set_Global.LAST_VISIT_TOKEN, TID);
      end if;

      --  Add forum information into the translate set
      --  ??? we could probably cache those values

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (Fid => V2P.Context.Counter.Get_Value
              (Context => Context.all,
               Name    => Template_Defs.Set_Global.FID),
            Tid => TID));
   exception
      when Database.Parameter_Error | Constraint_Error =>
         raise Error_404;
   end Forum_Entry_Internal;

   -------------------
   -- Forum_Entry_P --
   -------------------

   procedure Forum_Entry_P
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Parameters   :                 Callback_Parameters;
      Translations : in out          Templates.Translate_Set)
   is
      TID : constant Database.Id :=
              Database.Id'Value
                (Strings.Unbounded.To_String (Parameters (1)));
   begin
      Forum_Entry_Internal (Request, Context, Translations, TID, False);
   end Forum_Entry_P;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      P    : constant Parameters.List := Status.Parameters (Request);
      FID  : constant Database.Id :=
               Database.Id'Value
                 (Parameters.Get
                    (P, Template_Defs.Page_Forum_Threads.HTTP.FID));
      From : Natural;
   begin

      if Parameters.Exist (P, Template_Defs.Block_Forum_List.HTTP.FROM) then
         From := Positive'Value
           (Parameters.Get
              (P, Template_Defs.Block_Forum_List.HTTP.FROM));
      else
         From := 0;
      end if;
      Forum_Threads_Internal (Request, Context, Translations, FID, From);
   end Forum_Threads;

   ----------------------------
   -- Forum_Threads_Internal --
   ----------------------------

   procedure Forum_Threads_Internal
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set;
      FID          : in              Database.Id;
      From         : in              Natural)
   is
      pragma Unreferenced (Request);
      Cur_FID : constant Natural :=
                  V2P.Context.Counter.Get_Value
                    (Context => Context.all,
                     Name    => Template_Defs.Set_Global.FID);
      Login   : constant String :=
                  Context.Get_Value (Template_Defs.Set_Global.LOGIN);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Context.Remove (Template_Defs.Set_Global.TID);
      end if;

      --  Set forum Id into the context, and clear current filter if changing
      --  forum.

      if Cur_FID /= FID then
         V2P.Context.Counter.Set_Value
           (Context => Context.all,
            Name    => Template_Defs.Set_Global.FID,
            Value   => FID);

         Context.Remove (Template_Defs.Set_Global.FILTER_CATEGORY);
      end if;

      if From > 0 then
         V2P.Context.Not_Null_Counter.Set_Value
           (Context.all, Template_Defs.Set_Global.NAV_FROM, From);

      elsif not Context.Exist (Template_Defs.Set_Global.NAV_FROM) then
         --  Default is to start to first post when entering a forum
         V2P.Context.Not_Null_Counter.Set_Value
           (Context.all, Template_Defs.Set_Global.NAV_FROM, 1);
      end if;

      Templates.Insert
        (Translations, Database.Get_Forum (FID, Tid => Database.Empty_Id));

      --  Update last_forum_visit table
      if Login /= "" then
         Database.Set_Last_Forum_Visit (Login, FID);
      end if;
   exception
      when Database.Parameter_Error =>
         --  Redirect to main page
         --  ??? Log the exception message ?
         --  ??? Raise 404 Error
         raise Error_404;
   end Forum_Threads_Internal;

   ---------------------
   -- Forum_Threads_P --
   ---------------------

   procedure Forum_Threads_P
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Parameters   :                 Callback_Parameters;
      Translations : in out          Templates.Translate_Set) is
   begin
      Forum_Threads_Internal
        (Request, Context, Translations,
         Database.Id'Value (Strings.Unbounded.To_String (Parameters (1))), 0);
   end Forum_Threads_P;

   ----------
   -- Main --
   ----------

   procedure Main
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Context.Remove (Template_Defs.Set_Global.TID);
      end if;

      if Context.Exist (Template_Defs.Set_Global.FID) then
         Context.Remove (Template_Defs.Set_Global.FID);
      end if;

      Templates.Insert (Translations, Database.Get_Stats);
   end Main;

   ----------------
   -- New_Avatar --
   ----------------

   procedure New_Avatar
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Ada.Strings.Unbounded;
      use Image.Data;

      function Rand10 return String;
      --  Returns a 10 digits random number

      ------------
      -- Rand10 --
      ------------

      function Rand10 return String is
         R : String (1 .. 10);
      begin
         AWS.Utils.Random_String (R);
         return R;
      end Rand10;

      Login      : constant String :=
                     Context.Get_Value (Template_Defs.Set_Global.LOGIN);

      P          : constant Parameters.List := Status.Parameters (Request);

      Filename   : constant String :=
                     Parameters.Get
                       (P,
                        Template_Defs.Block_Pref_New_Avatar.HTTP.bpa_avatar);
      C_Filename : constant String :=
                     Directories.Compose
                       (Directories.Containing_Directory (Filename),
                        Strings.Fixed.Translate
                          (Source  => Login & Rand10,
                           Mapping => Utils.Clean_Mapping'Access),
                        Directories.Extension (Filename));

      Prefs      : Database.User_Settings;
   begin
      Database.Preference.User (Login, Prefs);

      --  First rename the file to be compatible with ImageMagick

      if Directories.Exists (C_Filename) then
         Directories.Delete_File (C_Filename);
      end if;

      Directories.Rename (Old_Name => Filename, New_Name => C_Filename);

      --  If a new photo has been uploaded, insert it in the database

      if C_Filename /= "" then
         New_Avatar : declare
            New_Image : Image_Data;
         begin
            Store_Avatar (Img      => New_Image,
                          Root_Dir => Gwiad_Plugin_Path,
                          Filename => C_Filename);

            if New_Image.Init_Status = Image_Created then
               Set_Pref : declare
                  use URL;
                  Filename     : constant String := New_Image.Filename;
                  New_Filename : constant String := Filename
                    (Filename'First + Avatar_Full_Prefix'Length + 1
                     .. Filename'Last);
               begin
                  Database.Preference.Set_Avatar (Login, New_Filename);

                  Templates.Insert
                    (Translations, Templates.Assoc
                       (Template_Defs.Block_User_Avatar.USER_AVATAR,
                        New_Filename));

                  --  Delete old avatar if any

                  if Prefs.Avatar /= Null_Unbounded_String then
                     declare
                        Old_Avatar : constant String := Directories.Compose
                          (Containing_Directory =>
                             Morzhol.OS.Compose
                               (Gwiad_Plugin_Path,
                                Settings.Get_Avatars_Path),
                           Name                 => To_String (Prefs.Avatar));
                     begin
                        Directories.Delete_File (Old_Avatar);
                     end;
                  end if;
               end Set_Pref;
            end if;
         end New_Avatar;

         Directories.Delete_File (C_Filename);
      end if;
   end New_Avatar;

   ---------------------
   -- New_Photo_Entry --
   ---------------------

   procedure New_Photo_Entry
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      use Image.Data;

      package Post_Entry renames Template_Defs.Page_Forum_New_Photo_Entry;

      P          : constant Parameters.List := Status.Parameters (Request);
      Filename   : constant String :=
                     Parameters.Get
                       (P, Template_Defs.Page_Photo_Post.HTTP.FILENAME);
      C_Filename : constant String :=
                     Strings.Fixed.Translate
                       (Source  => Filename,
                        Mapping => Utils.Clean_Mapping'Access);

      Login      : constant String :=
                     Context.Get_Value (Template_Defs.Set_Global.LOGIN);

   begin
      --  First rename the file to be compatible with ImageMagick

      if Filename /= C_Filename then
         if Directories.Exists (C_Filename) then
            Directories.Delete_File (C_Filename);
         end if;

         Directories.Rename
           (Old_Name => Filename,
            New_Name => C_Filename);
      end if;

      --  If a new photo has been uploaded, insert it in the database

      if C_Filename /= "" then
         New_Photo :
         declare
            New_Image : Image_Data;
         begin
            Init (Img      => New_Image,
                  Root_Dir => Gwiad_Plugin_Path,
                  Filename => C_Filename);

            if New_Image.Init_Status /= Image_Created then
               Templates.Insert
                 (Translations,
                  Templates.Assoc (Post_Entry.V2P_ERROR,
                    Image_Init_Status'Image (New_Image.Init_Status)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Post_Entry.EXCEED_MAXIMUM_IMAGE_DIMENSION,
                     Image_Init_Status'Image
                       (Image.Data.Exceed_Max_Image_Dimension)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Post_Entry.EXCEED_MAXIMUM_SIZE,
                     Image_Init_Status'Image (Image.Data.Exceed_Max_Size)));

            else
               Insert_Photo : declare
                  use URL;

                  --  Removes Images_Full_Prefix from New_Image.Filename

                  Filename           : constant String := New_Image.Filename;

                  New_Photo_Filename : constant String := Filename
                    (Filename'First + Big_Images_Full_Prefix'Length + 1
                     .. Filename'Last);
                  Pid                : constant String
                    := Database.Insert_Photo
                      (Uid      => Login,
                       Filename => New_Photo_Filename,
                       Height   => Natural (New_Image.Dimension.Height),
                       Width    => Natural (New_Image.Dimension.Width),
                       Medium_Height =>
                         Natural (New_Image.Dimension.Medium_Height),
                       Medium_Width  =>
                         Natural (New_Image.Dimension.Medium_Width),
                       Thumb_Height =>
                         Natural (New_Image.Dimension.Thumb_Height),
                       Thumb_Width  =>
                         Natural (New_Image.Dimension.Thumb_Width),
                       Size     => Natural (New_Image.Dimension.Size));
               begin
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Page_Forum_New_Photo_Entry.PID, Pid));
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Page_Forum_New_Photo_Entry.IMAGE_SOURCE,
                        New_Photo_Filename));
               end Insert_Photo;
            end if;
         end New_Photo;

         Directories.Delete_File (C_Filename);

      else
         if Context.Exist (Template_Defs.Set_Global.HAS_POST_PHOTO) then
            --  Display last uploaded photo

            Templates.Insert
              (Translations, Database.Get_User_Last_Photo (Login));
            Context.Remove (Template_Defs.Set_Global.HAS_POST_PHOTO);
         end if;
      end if;
   end New_Photo_Entry;

   ----------------
   -- Post_Photo --
   ----------------

   procedure Post_Photo
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
      Login : constant String :=
                Context.Get_Value (Template_Defs.Set_Global.LOGIN);
   begin
      if Login /= "" then
         if not Context.Exist (Template_Defs.Set_Global.HAS_POST_PHOTO) then
            Context.Set_Value
              (Template_Defs.Set_Global.HAS_POST_PHOTO, Boolean'Image (True));
         end if;

         Templates.Insert
           (Translations,
            Database.Get_New_Post_Delay
              (Uid => Login,
               TZ  => Context.Get_Value (Template_Defs.Set_Global.TZ)));
         Templates.Insert (Translations, Database.Get_User_Last_Photo (Login));

         if Context.Exist (Template_Defs.Set_Global.TID) then
            Context.Remove (Template_Defs.Set_Global.TID);
         end if;
      end if;
   end Post_Photo;

   -----------------------
   -- RSS_Last_Comments --
   -----------------------

   procedure Rss_Last_Comments
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.Set.URL));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Comments.V2P_URL,
            V2P.Settings.RSS_Host_URL));

      Templates.Insert
        (Translations,
         Database.Get_Latest_Comments (Limit => Settings.RSS_Latest_Comments));
   end Rss_Last_Comments;

   ---------------------
   -- Rss_Last_Photos --
   ---------------------

   procedure Rss_Last_Photos
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      P : constant Parameters.List := Status.Parameters (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.Set.URL));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.THUMB_SOURCE_PREFIX,
            Settings.Thumbs_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Posts.V2P_URL,
            V2P.Settings.RSS_Host_URL));

      Templates.Insert
        (Translations,
         Database.Get_Latest_Posts
           (Limit      => Settings.RSS_Latest_Posts,
            Add_Date   => True,
            TZ         => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Photo_Only => True,
            From_User  => Parameters.Get (P, "from")));
   end Rss_Last_Photos;

   --------------------
   -- Rss_Last_Posts --
   --------------------

   procedure Rss_Last_Posts
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Set_Global.FORUM_ENTRY_URL,
            Template_Defs.Page_Forum_Entry.Set.URL));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Set_Global.THUMB_SOURCE_PREFIX,
            Settings.Thumbs_Source_Prefix));

      Templates.Insert
        (Translations, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Posts.V2P_URL,
            V2P.Settings.RSS_Host_URL));

      Templates.Insert
        (Translations,
         Database.Get_Latest_Posts
           (Limit         => Settings.RSS_Latest_Posts,
            Add_Date      => True,
            TZ            => Context.Get_Value (Template_Defs.Set_Global.TZ),
            Show_Category => True));
   end Rss_Last_Posts;

   ----------
   -- User --
   ----------

   procedure User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Page_User.USER_NAME, User_Name));
   end User;

   -----------
   -- Users --
   -----------

   procedure Users
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      Context.Remove (Template_Defs.Set_Global.NAV_FROM);
   end Users;

   ------------------------
   -- Validate_New_Email --
   ------------------------

   procedure Validate_New_Email
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P     : constant Parameters.List := Status.Parameters (Request);
      Login : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.LOGIN);
      Key   : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.KEY);
   begin
      if not Database.Registration.Validate_New_User_Email (Login, Key) then
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Page_Validate_New_Email.ERROR, True));
      end if;
   end Validate_New_Email;

   -------------------
   -- Validate_User --
   -------------------

   procedure Validate_User
     (Request      : in              Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P     : constant Parameters.List := Status.Parameters (Request);
      Login : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.LOGIN);
      Key   : constant String :=
                Parameters.Get (P, Template_Defs.Set_Global.KEY);
   begin
      if not Database.Registration.Validate_User (Login, Key) then
         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Page_Validate_User.ERROR, True));
      end if;
   end Validate_User;

end V2P.Callbacks.Page;
