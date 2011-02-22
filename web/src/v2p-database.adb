------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2011                          --
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
with Ada.Exceptions;

with AWS.Utils;

with Image.Metadata.Embedded;
with Morzhol.Logs;
with Morzhol.OS;
with Morzhol.Strings;

with V2P.Database.Preference;
with V2P.Database.Timezone;
with V2P.DB_Handle;
with V2P.Settings;

with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Page_Main;
with V2P.Template_Defs.Page_Rss_Last_Comments;
with V2P.Template_Defs.Page_Rss_Last_Photos;
with V2P.Template_Defs.Page_Rss_Last_Posts;
with V2P.Template_Defs.Chunk_Comment;
with V2P.Template_Defs.Chunk_Forum_Category;
with V2P.Template_Defs.Chunk_List_Navlink;
with V2P.Template_Defs.Chunk_Threads_List;
with V2P.Template_Defs.Chunk_Threads_Text_List;
with V2P.Template_Defs.Chunk_Users;
with V2P.Template_Defs.Block_Comments;
with V2P.Template_Defs.Block_Exif;
with V2P.Template_Defs.Block_Forum_Threads;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Latest_Posts;
with V2P.Template_Defs.Block_Latest_Users;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_User_Stats;
with V2P.Template_Defs.Block_User_Comment_List;
with V2P.Template_Defs.Block_User_Photo_List;
with V2P.Template_Defs.Set_Global;

with V2P.Template_Defs.R_Block_Forum_List;

private with V2P.Database.Support;

package body V2P.Database is

   use Ada;
   use Ada.Exceptions;

   use Morzhol;
   use Morzhol.Strings;
   use Morzhol.OS;
   use V2P.Database.Support;

   use V2P.Template_Defs;

   Module : constant Logs.Module_Name := "Database";

   type User_Stats is record
      Created        : Unbounded_String;
      Last_Connected : Unbounded_String;
      N_Photos       : Natural;
      N_Messages     : Natural;
      N_Comments     : Natural;
      N_CdC          : Natural;
   end record;

   function Get_Fid
     (DBH      : in TLS_DBH_Access;
      Fid, Tid : in Id) return Id;
   pragma Inline (Get_Fid);
   --  Returns Fid if not empty otherwise compute it using Tid

   function Get_Fid_From_Category
     (DBH : in TLS_DBH_Access;
      Cid : in Id) return Id;
   pragma Inline (Get_Fid_From_Category);
   --  Returns the Fid given the category

   function Get_User_Stats (Uid, TZ : in String) return User_Stats;
   --  Returns stats about the specified user

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in TLS_DBH_Access) is
      DB_Path : constant String :=
                  Morzhol.OS.Compose (Gwiad_Plugin_Path, Settings.Get_DB_Name);
   begin
      if not DBH.Connected then
         if Directories.Exists (Name => DB_Path) then
            DBH.Handle := new DB.Handle'Class'(DB_Handle.Get);
            DBH.Handle.Connect (DB_Path);
            DBH.Connected := True;
            DBH_TLS.Set_Value (DBH.all);
         else
            Logs.Write
              (Name    => Module,
               Kind    => Logs.Error,
               Content => "ERROR : No database found : " & DB_Path);
            raise No_Database
              with "ERROR : No database found : " & DB_Path;
         end if;
      end if;
   end Connect;

   --------------------
   -- Get_Categories --
   --------------------

   function Get_Categories (Fid : in Id) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Templates.Tag;
      Name : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT id, name FROM category"
         & " WHERE forum_id=" & To_String (Fid)
         & " ORDER BY name");

      while Iter.More loop
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.CATEGORY_ID, Id));
      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.CATEGORY, Name));

      return Set;
   end Get_Categories;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Tid : in Id) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Templates.Tag;
      Name : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT id, name FROM category"
         & " WHERE post.category_id=category.id "
         & " AND post.id=" & To_String (Tid));

      if Iter.More then
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end if;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.CATEGORY, Name));

      return Set;
   end Get_Category;

   ----------------------------
   -- Get_Category_Full_Name --
   ----------------------------

   function Get_Category_Full_Name (CID : in String) return String is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Name : Unbounded_String;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT f.name, c.name FROM category c, forum f "
         & "WHERE f.id=c.forum_id AND c.id=" & Q (CID));

      if Iter.More then
         Iter.Get_Line (Line);

         Name := To_Unbounded_String
           (Directories.Compose
              (Containing_Directory => DB.String_Vectors.Element (Line, 1),
               Name                 => DB.String_Vectors.Element (Line, 2)));
         Line.Clear;
      end if;

      Iter.End_Select;

      return To_String (Name);
   end Get_Category_Full_Name;

   -----------------
   -- Get_Comment --
   -----------------

   function Get_Comment
     (Cid : in Id; TZ : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT strftime('%Y-%m-%dT%H:%M:%SZ', "
         & Timezone.Date_Time ("date", TZ) & "), "
         & Timezone.Date ("date", TZ) & ", " & Timezone.Time ("date", TZ)
         & ", user_login, anonymous_user, "
         & "comment, "
         & "(SELECT filename FROM photo WHERE id=comment.photo_id), has_voted"
         & " FROM comment WHERE id=" & To_String (Cid));

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.COMMENT_ID, Cid));

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.DATE_ISO_8601,
               DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc (Template_Defs.Chunk_Comment.DATE,
            DB.String_Vectors.Element (Line, 2)));

         Templates.Insert
           (Set, Templates.Assoc (Template_Defs.Chunk_Comment.TIME,
            DB.String_Vectors.Element (Line, 3)));

         Templates.Insert
           (Set, Templates.Assoc (Template_Defs.Chunk_Comment.USER,
            DB.String_Vectors.Element (Line, 4)));

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.ANONYMOUS_USER,
               DB.String_Vectors.Element (Line, 5)));

         Templates.Insert
           (Set, Templates.Assoc (Template_Defs.Chunk_Comment.COMMENT,
            DB.String_Vectors.Element (Line, 6)));

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.COMMENT_IMAGE_SOURCE,
               DB.String_Vectors.Element (Line, 7)));

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.HAS_VOTED,
               DB.String_Vectors.Element (Line, 8)));

         Line.Clear;
      end if;

      Iter.End_Select;
      return Set;
   end Get_Comment;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
     (Tid : in Id; Login, TZ : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH            : constant TLS_DBH_Access :=
                         TLS_DBH_Access (DBH_TLS.Reference);
      Set            : Templates.Translate_Set;
      Iter           : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line           : DB.String_Vectors.Vector;
      Date_Revealed  : Unbounded_String;
      First_Revealed : Unbounded_String;

      Comment_Id         : Templates.Tag;
      Comment_Level      : Templates.Tag;
      Nb_Levels_To_Close : Templates.Tag;
      User               : Templates.Tag;
      Anonymous          : Templates.Tag;
      Date_Iso_8601      : Templates.Tag;
      Date               : Templates.Tag;
      Time               : Templates.Tag;
      Comment            : Templates.Tag;
      Filename           : Templates.Tag;
      Has_Voted          : Templates.Tag;
      Is_New             : Templates.Tag;
      Photo_Number       : Templates.Tag;
      Photo_Index        : Positive := 2;
      --  Start at index 2, the number 1 is the original photo posted. The
      --  other attachments are 2, 3, etc.

      function Select_Is_New return String;
      pragma Inline (Select_Is_New);
      --  Returns true (1) if the post is new since last user visit.
      --  If last_activity is older that 30 days it is ignored. Note that we
      --  are using <= as the last visit date is set after we have read the
      --  post data. So we really want to catch comments posted at the same
      --  time we have read the comments.

      -------------------
      -- Select_Is_New --
      -------------------

      function Select_Is_New return String is
      begin
         if Login /= "" then
            return ", (comment.date>DATE ('now', '-30 days')"
              & "   AND (COALESCE ((SELECT v.last_activity"
              & " FROM last_user_visit v"
              & " WHERE v.post_id=post_comment.post_id"
              & "   AND v.user_login=" & Q (Login)
              & " ), DATE('now', '-30 days')) < comment.date))";
         else
            return "";
         end if;
      end Select_Is_New;

   begin
      Connect (DBH);

      --  Get date post to set the DATE_FIRST_REVEALED

      DBH.Handle.Prepare_Select
        (Iter, "SELECT JULIANDAY(date_post, '+"
         & Utils.Image (Settings.Anonymity_Hours) & " hours')"
         & " FROM post WHERE id=" & To_String (Tid));

      if Iter.More then
         Iter.Get_Line (Line);
         Date_Revealed := +DB.String_Vectors.Element (Line, 1);
      end if;

      Line.Clear;

      --  Get comments

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT comment.id, strftime('%Y-%m-%dT%H:%M:%SZ', "
         & Timezone.Date_Time ("date", TZ) & "), " & Timezone.Date ("date", TZ)
         & ", " & Timezone.Time ("date", TZ) & ", "
         & "user_login, anonymous_user, comment, "
         & "(SELECT filename FROM photo WHERE id=comment.photo_id), has_voted,"
         & " JULIANDAY(date)"
         & Select_Is_New
         & " FROM comment, post_comment"
         & " WHERE post_id=" & To_String (Tid)
         & " AND post_comment.comment_id=comment.id");

      while Iter.More loop
         Iter.Get_Line (Line);

         declare
            Id   : constant String := DB.String_Vectors.Element (Line, 1);
            File : constant String :=
                     DB.String_Vectors.Element (Line, 8);
         begin
            if DB.String_Vectors.Element (Line, 10) > -Date_Revealed
              and then First_Revealed = Null_Unbounded_String
            then
               First_Revealed := +Id;
            end if;

            Comment_Id    := Comment_Id & Id;
            Date_Iso_8601 := Date_Iso_8601
              & DB.String_Vectors.Element (Line, 2);
            Date          := Date & DB.String_Vectors.Element (Line, 3);
            Time          := Time & DB.String_Vectors.Element (Line, 4);
            User          := User
              & DB.String_Vectors.Element (Line, 5);
            Anonymous     := Anonymous
              & DB.String_Vectors.Element (Line, 6);
            Comment       := Comment
              & DB.String_Vectors.Element (Line, 7);

            Filename := Filename & File;

            if File = "" then
               Photo_Number := Photo_Number & "";
            else
               Photo_Number := Photo_Number & Utils.Image (Photo_Index);
               Photo_Index := Photo_Index + 1;
            end if;
         end;

         Has_Voted := Has_Voted & DB.String_Vectors.Element (Line, 9);

         if Login /= "" then
            Is_New := Is_New & (DB.String_Vectors.Element (Line, 11) /= "0");
         end if;

         --  Unthreaded view

         Comment_Level      := Comment_Level      & 1;
         Nb_Levels_To_Close := Nb_Levels_To_Close & 1;

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Comments.COMMENT_ID, Comment_Id));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.COMMENT_IMAGE_SOURCE, Filename));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.COMMENT_IMAGE_INDEX, Photo_Number));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.DATE_ISO_8601, Date_Iso_8601));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.DATE, Date));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.TIME, Time));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.USER, User));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.ANONYMOUS_USER, Anonymous));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.COMMENT, Comment));
      Templates.Insert
        (Set, Templates.Assoc (Block_Comments.COMMENT_LEVEL, Comment_Level));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_Comments.NB_LEVELS_TO_CLOSE, Nb_Levels_To_Close));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Comment.HAS_VOTED, Has_Voted));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Comments.ID_FIRST_REVEALED, First_Revealed));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Comment.IS_NEW, Is_New));

      return Set;
   end Get_Comments;

   --------------
   -- Get_Exif --
   --------------

   function Get_Exif (Tid : in Id) return Templates.Translate_Set is

      function "+"
        (Str : in String) return Unbounded_String renames To_Unbounded_String;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;

      Exif : Image.Metadata.Embedded.Data;

   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT create_date, make, camera_model_name, "
         & "shutter_speed_value, aperture_value, flash, focal_length, "
         & "exposure_mode, exposure_program, white_balance, metering_mode, "
         & "iso FROM photo_exif "
         & "WHERE photo_id=(SELECT photo_id FROM post WHERE id="
         & To_String (Tid)
         & ')');

      if Iter.More then
         Iter.Get_Line (Line);

         Exif := Image.Metadata.Embedded.Data'
           (Create_Date         => +DB.String_Vectors.Element (Line, 1),
            Make                => +DB.String_Vectors.Element (Line, 2),
            Camera_Model_Name   => +DB.String_Vectors.Element (Line, 3),
            Shutter_Speed_Value => +DB.String_Vectors.Element (Line, 4),
            Aperture_Value      => +DB.String_Vectors.Element (Line, 5),
            Flash               => +DB.String_Vectors.Element (Line, 6),
            Focal_Length        => +DB.String_Vectors.Element (Line, 7),
            Exposure_Mode       => +DB.String_Vectors.Element (Line, 8),
            Exposure_Program    => +DB.String_Vectors.Element (Line, 9),
            White_Balance       => +DB.String_Vectors.Element (Line, 10),
            Metering_Mode       => +DB.String_Vectors.Element (Line, 11),
            ISO                 => +DB.String_Vectors.Element (Line, 12));

         Iter.End_Select;

      else
         --  No exif metadata recorded for this photo, get them now
         DBH.Handle.Prepare_Select
           (Iter, "SELECT filename FROM photo WHERE id="
            & "(SELECT photo_id FROM post WHERE id="
            & To_String (Tid)
            & ')');

         if Iter.More then
            Iter.Get_Line (Line);

            Exif := Image.Metadata.Embedded.Get
              (Morzhol.OS.Compose
                 (Gwiad_Plugin_Path,
                  Settings.Get_Big_Images_Path & Directory_Separator
                  & DB.String_Vectors.Element (Line, 1)));
         end if;

         DBH.Handle.Execute
           ("INSERT INTO photo_exif " &
            "('photo_id', 'create_date', 'make', 'camera_model_name', "
            & "'shutter_speed_value', 'aperture_value', 'flash', "
            & "'focal_length', 'exposure_mode', 'exposure_program', "
            & "'white_balance', 'metering_mode', 'iso') "
            & "VALUES ((SELECT photo_id FROM post WHERE id="
            & To_String (Tid) & ")," & Q (Exif.Create_Date)
            & ',' & Q (Exif.Make) & ','
            & Q (Exif.Camera_Model_Name) & ',' & Q (Exif.Shutter_Speed_Value)
            & ',' & Q (Exif.Aperture_Value) & ',' & Q (Exif.Flash) & ','
            & Q (Exif.Focal_Length) & ',' & Q (Exif.Exposure_Mode) & ','
            & Q (Exif.Exposure_Program) & ',' & Q (Exif.White_Balance) & ','
            & Q (Exif.Metering_Mode) & ',' & Q (Exif.ISO) & ')');
      end if;

      Templates.Insert
        (Set, Templates.Assoc (Block_Exif.EXIF_ISO, Exif.ISO));
      Templates.Insert
        (Set, Templates.Assoc (Block_Exif.EXIF_CREATE_DATE, Exif.Create_Date));
      Templates.Insert
        (Set, Templates.Assoc (Block_Exif.EXIF_MAKE, Exif.Make));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_CAMERA_MODEL_NAME, Exif.Camera_Model_Name));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_SHUTTER_SPEED_VALUE, Exif.Shutter_Speed_Value));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_APERTURE_VALUE, Exif.Aperture_Value));
      Templates.Insert
        (Set, Templates.Assoc (Block_Exif.EXIF_FLASH, Exif.Flash));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_FOCAL_LENGTH, Exif.Focal_Length));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_EXPOSURE_MODE, Exif.Exposure_Mode));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_EXPOSURE_PROGRAM, Exif.Exposure_Program));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_WHITE_BALANCE, Exif.White_Balance));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Exif.EXIF_METERING_MODE, Exif.Metering_Mode));

      return Set;
   end Get_Exif;

   -------------
   -- Get_Fid --
   -------------

   function Get_Fid
     (DBH      : in TLS_DBH_Access;
      Fid, Tid : in Id) return Id
   is
      Line : DB.String_Vectors.Vector;
   begin
      if Fid = Empty_Id then
         --  Get the Fid using Tid
         Check_Fid : declare
            Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
         begin
            DBH.Handle.Prepare_Select
              (Iter,
               "SELECT forum_id FROM category, post "
                 & "WHERE category.id=post.category_id "
                 & "AND post.id=" & To_String (Tid));

            if Iter.More then
               Iter.Get_Line (Line);

               Read_Fid : declare
                  Fid : constant Id :=
                          Id'Value (DB.String_Vectors.Element (Line, 1));
               begin
                  Line.Clear;
                  Iter.End_Select;
                  return Fid;
               end Read_Fid;

            else
               Logs.Write
                 (Name    => Module,
                  Kind    => Logs.Error,
                  Content => "Get_Fid, Fid and Tid empty, "
                    & "raise Database_Error");
               raise Database_Error;
            end if;
         end Check_Fid;

      else
         return Fid;
      end if;
   end Get_Fid;

   ---------------------------
   -- Get_Fid_From_Category --
   ---------------------------

   function Get_Fid_From_Category
     (DBH : in TLS_DBH_Access;
      Cid : in Id) return Id
   is
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT forum_id FROM category "
           & "WHERE category.id=" & To_String (Cid));

      if Iter.More then
         Iter.Get_Line (Line);

         Return_Fid : declare
            Fid : constant Id :=
                    Id'Value (DB.String_Vectors.Element (Line, 1));
         begin
            Line.Clear;
            Iter.End_Select;
            return Fid;
         end Return_Fid;

      else
         Logs.Write
           (Name    => Module,
            Kind    => Logs.Error,
            Content => "Get_Fid_From_Category, Cid does not exist, "
              & "raise Database_Error");
         raise Database_Error;
      end if;
   end Get_Fid_From_Category;

   ---------------
   -- Get_Forum --
   ---------------

   function Get_Forum (Fid, Tid : in Id) return Templates.Translate_Set is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;
   begin
      Connect (DBH);

      Get_Forum_Data : declare
         L_Fid : constant Id := Get_Fid (DBH, Fid, Tid);
         --  Local Fid computed using Fid or Tid
      begin
         DBH.Handle.Prepare_Select
           (Iter,
            "SELECT name, anonymity, for_photo FROM forum WHERE id="
            & To_String (L_Fid));

         if Iter.More then
            Iter.Get_Line (Line);

            Forum_Data : declare
               Name      : constant String  :=
                             DB.String_Vectors.Element (Line, 1);
               Anonymity : constant String :=
                             DB.String_Vectors.Element (Line, 2);
               For_Photo : constant String :=
                             DB.String_Vectors.Element (Line, 3);
            begin
               Line.Clear;
               Iter.End_Select;

               Templates.Insert
                 (Set, Templates.Assoc (Block_Forum_List.FORUM_NAME, Name));
               Templates.Insert
                 (Set,
                  Templates.Assoc
                    (Page_Forum_Entry.FORUM_ANONYMITY, Anonymity));
               Templates.Insert
                 (Set, Templates.Assoc
                    (Page_Forum_Threads.FORUM_FOR_PHOTO, For_Photo));
               Templates.Insert (Set, Templates.Assoc (Set_Global.FID, L_Fid));
            end Forum_Data;

         else
            Iter.End_Select;
            raise Parameter_Error with "Can not find forum FID= "
              & To_String (Fid) & " TID=" & To_String (Tid);
         end if;
      end Get_Forum_Data;

      return Set;
   end Get_Forum;

   ------------------
   -- Get_Forum_Id --
   ------------------

   function Get_Forum_Id (Tid : in Id) return Id is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      return Get_Fid (DBH, Empty_Id, Tid);
   end Get_Forum_Id;

   --------------------
   -- Get_Forum_Type --
   --------------------

   function Get_Forum_Type (Tid : in Id) return V2P.Database.Forum_Type is
      DBH        : constant TLS_DBH_Access :=
                     TLS_DBH_Access (DBH_TLS.Reference);
      Iter       : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line       : DB.String_Vectors.Vector;
      Forum_Type : V2P.Database.Forum_Type := Forum_Text;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT for_photo FROM category, post, forum "
         & "WHERE category.id=post.category_id "
         & "AND forum.id=category.forum_id "
         & "AND post.id=" & To_String (Tid));

      if not Iter.More then
         Logs.Write
           (Name    => Module,
            Kind    => Logs.Error,
            Content => "Get_Id, Fid and Tid empty, raise Parameter_Error");
         raise Parameter_Error
           with "Can not get forum type for Tid = " & To_String (Tid);
      end if;

      Iter.Get_Line (Line);

      if DB.String_Vectors.Element (Line, 1) = "TRUE" then
         Forum_Type := Forum_Photo;
      end if;

      Line.Clear;

      Iter.End_Select;
      return Forum_Type;
   end Get_Forum_Type;

   ----------------
   -- Get_Forums --
   ----------------

   function Get_Forums
     (Filter : in Forum_Filter;
      TZ     : in String;
      Login  : in String)
      return Templates.Translate_Set
   is
      use type Templates.Tag;

      function Select_Is_New return String;
      --  Whether the forum should be mark !NEW.
      --  If Login is not set, do not mark as !NEW.

      -------------------
      -- Select_Is_New --
      -------------------

      function Select_Is_New return String is
      begin
         if Login /= "" then
            return "(SELECT COUNT(*) FROM post, last_forum_visit, category "
              & "WHERE post.id>last_forum_visit.last_post_id AND "
              & "last_forum_visit.user_login=" & Q (Login)
              & " AND last_forum_visit.forum_id=forum.id AND "
              & " post.category_id=category.id AND "
              & "category.forum_id=forum.id) > 0 ";
         else
            return "0";
         end if;
      end Select_Is_New;

      SQL       : constant String :=
                    "SELECT id, name, for_photo, "
                      & Timezone.Date ("last_activity", TZ) & ", "
                      & Timezone.Time ("last_activity", TZ) & ", "
                      & Select_Is_New
                      & " FROM forum";
      DBH       : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);

      Set       : Templates.Translate_Set;
      Iter      : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line      : DB.String_Vectors.Vector;
      Id        : Templates.Tag;
      Name      : Templates.Tag;
      For_Photo : Templates.Tag;
      Date      : Templates.Tag;
      Time      : Templates.Tag;
      Is_New    : Templates.Tag;
      Nb_Lines  : Natural := 0;

   begin
      Connect (DBH);

      if Filter /= Forum_All then
         DBH.Handle.Prepare_Select
           (Iter, SQL & " WHERE for_photo='"
            & Boolean'Image (Filter = Forum_Photo) & "'");
      else
         DBH.Handle.Prepare_Select (Iter, SQL);
      end if;

      while Iter.More loop
         Nb_Lines := Nb_Lines + 1;
         Iter.Get_Line (Line);

         Id        := Id        & DB.String_Vectors.Element (Line, 1);
         Name      := Name      & DB.String_Vectors.Element (Line, 2);
         For_Photo := For_Photo & DB.String_Vectors.Element (Line, 3);
         Date      := Date      & DB.String_Vectors.Element (Line, 4);
         Time      := Time      & DB.String_Vectors.Element (Line, 5);
         Is_New    := Is_New    & DB.String_Vectors.Element (Line, 6);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Forum_List.FID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.FORUM_NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.F_DATE, Date));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.F_TIME, Time));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.IS_NEW, Is_New));

      if Filter /= Forum_All and then Nb_Lines = 1 then
         --  Only one forum matched. Returns the categories too

         Templates.Insert
           (Set, Get_Categories (Database.Id'Value (Templates.Item (Id, 1))));
      end if;

      return Set;
   end Get_Forums;

   -------------------------
   -- Get_Latest_Comments --
   -------------------------

   function Get_Latest_Comments
     (Limit : in Positive) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

      TID        : Templates.Tag;
      Comment_Id : Templates.Tag;
      User       : Templates.Tag;
      Anonymous  : Templates.Tag;
      Date       : Templates.Tag;
      Comment    : Templates.Tag;
      Filename   : Templates.Tag;
      Revealed   : Templates.Tag;
      Owner      : Templates.Tag;
      Post_Photo : Templates.Tag;
      Anonymity  : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT post_comment.post_id, comment.id,"
         & " strftime('%Y-%m-%d %H:%M:%S', date), comment.user_login,"
         & " user_post.user_login, anonymous_user, comment.comment,"
         & " (SELECT filename FROM photo WHERE id=comment.photo_id),"
         & " DATETIME(post.date_post, '+"
         & Utils.Image (V2P.Settings.Anonymity_Hours)
         & " hour')<DATETIME('NOW'), "
         & " (SELECT filename FROM photo WHERE id=post.photo_id), anonymity "
         & " FROM comment, post_comment, post, user_post, forum, category "
         & " WHERE post_comment.comment_id=comment.id"
         & " AND has_voted='FALSE'"
         & " AND post.id=post_comment.post_id AND user_post.post_id=post.id"
         & " AND forum.id=category.forum_id "
         & " AND category.id=post.category_id "
         & " AND comment.id IN (SELECT comment.id FROM comment"
         & " WHERE has_voted='FALSE' ORDER BY comment.date DESC LIMIT"
         & I (Limit) & ") ORDER BY date DESC");

      while Iter.More loop
         Iter.Get_Line (Line);

         TID           := TID
           & DB.String_Vectors.Element (Line, 1);
         Comment_Id    := Comment_Id
           & DB.String_Vectors.Element (Line, 2);
         Date          := Date
           & DB.String_Vectors.Element (Line, 3);
         User          := User
           & DB.String_Vectors.Element (Line, 4);
         Owner        := Owner
           & DB.String_Vectors.Element (Line, 5);
         Anonymous     := Anonymous
           & DB.String_Vectors.Element (Line, 6);
         Comment       := Comment
           & DB.String_Vectors.Element (Line, 7);
         Filename      := Filename
           & DB.String_Vectors.Element (Line, 8);
         Revealed      := Revealed
           & DB.String_Vectors.Element (Line, 9);
         Post_Photo    := Post_Photo
           & DB.String_Vectors.Element (Line, 10);
         Anonymity     := Anonymity
           & DB.String_Vectors.Element (Line, 11);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Comments.TID, TID));

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Comments.COMMENT_ID, Comment_Id));

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.COMMENT_IMAGE_SOURCE, Filename));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.DATE, Date));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Chunk_Comment.USER, User));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.ANONYMOUS_USER, Anonymous));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.COMMENT, Comment));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.REVEALED, Revealed));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Chunk_Comment.OWNER, Owner));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Comments.POST_PHOTO, Post_Photo));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Rss_Last_Comments.ANONYMITY, Anonymity));

      return Set;
   end Get_Latest_Comments;

   ----------------------
   -- Get_Latest_Posts --
   ----------------------

   function Get_Latest_Posts
     (Limit         : in Positive;
      TZ            : in String;
      Add_Date      : in Boolean := False;
      Photo_Only    : in Boolean := False;
      From_User     : in String  := "";
      Show_Category : in Boolean := False) return Templates.Translate_Set
   is
      pragma Unreferenced (TZ);

      use type Templates.Tag;

      DBH      : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      Id       : Templates.Tag;
      Name     : Templates.Tag;
      Date     : Templates.Tag;
      Thumb    : Templates.Tag;
      Forum    : Templates.Tag;
      Category : Templates.Tag;
      Set      : Templates.Translate_Set;

      function Select_Date return String;
      --  Adds date selection if required

      -----------------
      -- Select_Date --
      -----------------

      function Select_Date return String is
      begin
         if Add_Date then
            return ", strftime('%Y-%m-%d %H:%M:%S', post.date_post)";
         else
            return "";
         end if;
      end Select_Date;

   begin
      Connect (DBH);

      --  Get entry information

      Prepare_Select : declare
         SQL : Unbounded_String :=
                 +"SELECT post.id, post.name, "
                   & "(SELECT filename FROM photo"
                   & " WHERE post.photo_id = photo.id)"
                   & Select_Date;
      begin
         if Show_Category then
            Append (SQL, ", category.name, forum.name ");
         end if;

         if not Photo_Only and not Show_Category then
            Append (SQL, " FROM post ");

         else
            Append (SQL, " FROM post, forum, category ");
            if From_User /= "" then
               Append (SQL, ", user_post ");
            end if;
         end if;

         if Photo_Only then
            Append (SQL, "WHERE post.category_id=category.id"
                    & " AND category.forum_id=forum.id"
                    & " AND forum.for_photo='TRUE'"
                    & " AND post.hidden='FALSE'");

            if From_User /= "" then
               Append (SQL,
                       " AND user_post.post_id=post.id AND user_login="
                       & Q (From_User));

               --  Do not display post for a specific user when it is not yet
               --  revealed.

               Append
                 (SQL, " AND DATETIME(post.date_post, '+"
                  & Utils.Image (Settings.Anonymity_Hours)
                  & " hour') < DATETIME('NOW')");
            end if;

         elsif Show_Category then
            Append (SQL, "WHERE post.category_id=category.id"
                    & " AND category.forum_id = forum.id"
                    & " AND post.hidden='FALSE' ");
         end if;

         Append
           (SQL, " ORDER BY post.date_post DESC "
            & "LIMIT " & Utils.Image (Limit));

         DBH.Handle.Prepare_Select (Iter, -SQL);
      end Prepare_Select;

      while Iter.More loop
         Iter.Get_Line (Line);

         Id    := Id    & DB.String_Vectors.Element (Line, 1);
         Name  := Name  & DB.String_Vectors.Element (Line, 2);
         Thumb := Thumb & DB.String_Vectors.Element (Line, 3);

         if Add_Date then
            Date  := Date  & DB.String_Vectors.Element (Line, 4);
            if Show_Category then
               Category := Category & DB.String_Vectors.Element (Line, 5);
               Forum    := Forum & DB.String_Vectors.Element (Line, 6);
            end if;
         else
            if Show_Category then
               Category := Category & DB.String_Vectors.Element (Line, 4);
               Forum    := Forum & DB.String_Vectors.Element (Line, 5);
            end if;
         end if;

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Latest_Posts.TID, Id));
      Templates.Insert (Set, Templates.Assoc (Block_Latest_Posts.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Block_Latest_Posts.THUMB_SOURCE, Thumb));

      if Add_Date then
         Templates.Insert
           (Set, Templates.Assoc (Page_Rss_Last_Posts.DATE, Date));
      end if;

      if Show_Category then
         Templates.Insert
           (Set,
           Templates.Assoc (Page_Rss_Last_Posts.POST_CATEGORY, Category));
         Templates.Insert
           (Set,
           Templates.Assoc (Page_Rss_Last_Posts.POST_FORUM, Forum));
      end if;

      Templates.Insert
        (Set, Templates.Assoc (Page_Rss_Last_Photos.FROM_USER, From_User));

      return Set;
   end Get_Latest_Posts;

   ----------------------
   -- Get_Latest_Users --
   ----------------------

   function Get_Latest_Users
     (Limit : in Positive) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String := "SELECT login FROM user "
                                   & " ORDER BY created DESC "
                                   & " LIMIT " & Positive'Image (Limit);
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      User  : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);

      --  Get entry information

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         User := User & DB.String_Vectors.Element (Line, 1);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Latest_Users.USER, User));

      return Set;
   end Get_Latest_Users;

   ------------------
   -- Get_Metadata --
   ------------------

   function Get_Metadata (Tid : in Id) return Templates.Translate_Set is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;

   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT geo_latitude, geo_longitude, "
         & "geo_latitude_formatted, geo_longitude_formatted "
         & "FROM photo_metadata "
         & "WHERE photo_id=(SELECT photo_id FROM post WHERE id="
         & To_String (Tid) & ')');

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Metadata.METADATA_LATITUDE,
               DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Metadata.METADATA_LONGITUDE,
               DB.String_Vectors.Element (Line, 2)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Metadata.METADATA_LATITUDE_FORMATTED,
               DB.String_Vectors.Element (Line, 3)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Metadata.METADATA_LONGITUDE_FORMATTED,
               DB.String_Vectors.Element (Line, 4)));

         Line.Clear;
      end if;

      Iter.End_Select;
      return Set;
   end Get_Metadata;

   ------------------------
   -- Get_New_Post_Delay --
   ------------------------

   function Get_New_Post_Delay
     (Uid : in String; TZ : in String) return Templates.Translate_Set
   is
      SQL : constant String := "SELECT JULIANDAY(p.date_post,'+"
        & Utils.Image (Settings.Posting_Delay_Hours)
        & " hour') - JULIANDAY('NOW'), "
        & Timezone.Date_Time ("DATETIME (p.date_post, '+"
                              &  Utils.Image (Settings.Posting_Delay_Hours)
                              & " hour')", TZ)
        & " FROM user_post up, post p "
        & " WHERE up.post_id=p.id AND p.photo_id!=0 "
        & "  AND DATETIME(p.date_post, '+"
        & Utils.Image (Settings.Posting_Delay_Hours)
        & " hour')>DATETIME('NOW') AND up.user_login=" & Q (Uid);

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set  => Set,
            Item => Templates.Assoc
              (Template_Defs.Set_Global.NEW_POST_DELAY,
               DB.String_Vectors.Element (Line, 1)));

         --  ??? Delay should be in day, hours, minutes...

         Templates.Insert
           (Set  => Set,
            Item => Templates.Assoc
              (Template_Defs.Set_Global.NEW_POST_DATE,
               DB.String_Vectors.Element (Line, 2)));

         Line.Clear;
      end if;

      Iter.End_Select;
      return Set;
   end Get_New_Post_Delay;

   --------------
   -- Get_Post --
   --------------

   function Get_Post
     (Tid        : in Id;
      Forum_Type : in V2P.Database.Forum_Type;
      TZ         : in String;
      Admin      : in     Boolean) return Templates.Translate_Set
   is
      function Post_Hidden return String;
      --  Returns SQL fragment with select hidden post depending on Admin

      -----------------
      -- Post_Hidden --
      -----------------

      function Post_Hidden return String is
      begin
         if Admin then
            return "";
         else
            return " AND post.hidden='FALSE'";
         end if;
      end Post_Hidden;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      --  Get entry information

      if Forum_Type = Forum_Photo then
         DBH.Handle.Prepare_Select
           (Iter, "SELECT post.name, post.comment, post.hidden, "
            & "filename, width, height, medium_width, medium_height, "
            & "thumb_width, thumb_height, user.login, "
            & Timezone.Date_Time ("post.date_post", TZ) & ", "
            & " (JULIANDAY(post.date_post, '+"
            & Utils.Image (Settings.Anonymity_Hours)
            & " hour') - JULIANDAY('NOW')) * 24, category.name, category.id, "
            & "(SELECT id FROM photo_of_the_week AS cdc "
            & " WHERE cdc.post_id=post.id) "
            & "FROM post, user, user_post, photo, category "
            & "WHERE post.id=" & To_String (Tid)
            & " AND user.login=user_post.user_login"
            & " AND user_post.post_id=post.id"
            & " AND photo.id=post.photo_id"
            & " AND category.id=post.category_id"
            & Post_Hidden);

         if Iter.More then
            Iter.Get_Line (Line);

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.NAME, DB.String_Vectors.Element (Line, 1)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.IMAGE_COMMENT,
                  DB.String_Vectors.Element (Line, 2)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.HIDDEN,
                  DB.String_Vectors.Element (Line, 3)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.IMAGE_SOURCE,
                  DB.String_Vectors.Element (Line, 4)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.IMAGE_WIDTH,
                  DB.String_Vectors.Element (Line, 5)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.IMAGE_HEIGHT,
                  DB.String_Vectors.Element (Line, 6)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.MEDIUM_IMAGE_WIDTH,
                  DB.String_Vectors.Element (Line, 7)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.MEDIUM_IMAGE_HEIGHT,
                  DB.String_Vectors.Element (Line, 8)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.THUMB_IMAGE_WIDTH,
                  DB.String_Vectors.Element (Line, 9)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.THUMB_IMAGE_HEIGHT,
                  DB.String_Vectors.Element (Line, 10)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.OWNER,
                  DB.String_Vectors.Element (Line, 11)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.DATE_POST,
                  DB.String_Vectors.Element (Line, 12)));

            Is_Revealed : declare
               Hours    : constant Float :=
                            Float'Value (DB.String_Vectors.Element (Line, 13));
               Revealed : Boolean;
            begin
               if Hours >= 0.0 then
                  Revealed := False;

                  Compute_Delay : declare
                     Delay_Hours : constant Natural :=
                                     Natural (Float'Floor (Hours));
                     Hours_Diff  : constant Float   :=
                                     Hours - Float'Floor (Hours);
                  begin
                     Templates.Insert
                       (Set, Templates.Assoc
                          (Page_Forum_Entry.DATE_REVEALED_HOURS,
                           Utils.Image (Delay_Hours)));
                     Templates.Insert
                       (Set, Templates.Assoc
                          (Page_Forum_Entry.DATE_REVEALED_MINUTES,
                           Utils.Image (Natural (Hours_Diff * 60.0))));
                  end Compute_Delay;

               else
                  Revealed := True;
               end if;

               Templates.Insert
                 (Set,
                  Templates.Assoc
                    (Page_Forum_Entry.REVEALED, Boolean'Image (Revealed)));
            end Is_Revealed;

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.CATEGORY,
                  DB.String_Vectors.Element (Line, 14)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Chunk_Forum_Category.CID,
                  DB.String_Vectors.Element (Line, 15)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.CDC,
                  DB.String_Vectors.Element (Line, 16) /= ""));

            Line.Clear;
         end if;

      else
         DBH.Handle.Prepare_Select
           (Iter, "SELECT post.name, post.comment, post.hidden, "
            & "user.login, " & Timezone.Date_Time ("post.date_post", TZ) & ", "
            & "DATETIME(post.date_post, '+"
            & Utils.Image (Settings.Anonymity_Hours)
            & " hour')<DATETIME('NOW'), category.name, category.id "
            & "FROM post, user, user_post, category "
            & "WHERE post.id=" & To_String (Tid)
            & " AND user.login=user_post.user_login"
            & " AND user_post.post_id=post.id"
            & " AND category.id=post.category_id"
            & Post_Hidden);

         if Iter.More then
            Iter.Get_Line (Line);

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.NAME, DB.String_Vectors.Element (Line, 1)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.IMAGE_COMMENT,
                  DB.String_Vectors.Element (Line, 2)));

            --  ??? IMAGE_COMMENT should be renamed

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.HIDDEN,
                  DB.String_Vectors.Element (Line, 3)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.OWNER,
                  DB.String_Vectors.Element (Line, 4)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.DATE_POST,
                  DB.String_Vectors.Element (Line, 5)));

            Templates.Insert
              (Set,
               Templates.Assoc
                 (Page_Forum_Entry.REVEALED,
                  DB.String_Vectors.Element (Line, 6)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.CATEGORY,
                  DB.String_Vectors.Element (Line, 7)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Chunk_Forum_Category.CID,
                  DB.String_Vectors.Element (Line, 8)));

            Line.Clear;
         end if;
      end if;

      Iter.End_Select;

      return Set;
   end Get_Post;

   ---------------
   -- Get_Stats --
   ---------------

   function Get_Stats return Templates.Translate_Set is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, "SELECT COUNT(*) from user");

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Page_Main.NB_USERS, DB.String_Vectors.Element (Line, 1)));
      end if;

      Iter.End_Select;

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT SUM(user_stats.nb_photo), SUM(user_stats.nb_com)"
         & " FROM user_stats;");

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Page_Main.NB_PHOTOS, DB.String_Vectors.Element (Line, 1)));
         Templates.Insert
           (Set, Templates.Assoc
              (Page_Main.NB_COMMENTS, DB.String_Vectors.Element (Line, 2)));
      end if;

      Iter.End_Select;

      return Set;
   end Get_Stats;

   -----------------
   -- Get_Threads --
   -----------------

   procedure Get_Threads
     (Fid           : in     Id := Empty_Id;
      Login         : in     String := "";
      User          : in     String := "";
      Admin         : in     Boolean;
      Forum         : in     Forum_Filter := Forum_All;
      Page_Size     : in     Navigation_Links.Page_Size :=
        Navigation_Links.Default_Page_Size;
      Filter        : in     Filter_Mode := All_Messages;
      Filter_Cat    : in     String      := "";
      Order_Dir     : in     Order_Direction := DESC;
      Sorting       : in     Forum_Sort := Last_Posted;
      Only_Revealed : in     Boolean := False;
      From          : in out Positive;
      Mode          : in     Select_Mode := Everything;
      Navigation    :    out Navigation_Links.Post_Ids.Vector;
      Set           :    out Templates.Translate_Set;
      Nb_Lines      :    out Natural;
      Total_Lines   :    out Natural;
      TZ            : in     String)
   is
      use type Templates.Tag;
      use type Navigation_Links.Post_Ids.Vector;

      function Build_Select
        (Count_Only : in Boolean := False) return String;
      --  Returns the SQL select

      function Build_From
        (User       : in String;
         Forum      : in Forum_Filter;
         Count_Only : in Boolean := False) return String;
      --  Returns the SQL from

      function Build_Where
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Forum      : in Forum_Filter;
         Count_Only : in Boolean := False) return String;
      --  Build the where statement

      function Count_Threads
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Forum      : in Forum_Filter) return Natural;
      --  Returns the number of threads matching the query

      function Threads_Ordered_Select
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         From       : in Positive;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Order_Dir  : in Order_Direction;
         Limit      : in Natural;
         Forum      : in Forum_Filter) return Unbounded_String;
      --  Returns the select SQL query for listing threads with Filter

      ----------------
      -- Build_From --
      ----------------

      function Build_From
        (User       : in String;
         Forum      : in Forum_Filter;
         Count_Only : in Boolean := False) return String
      is
         From_Stmt : Unbounded_String := +" FROM post, category";
      begin
         if not Count_Only or else User /= "" then
            --  Needed the user_post table for join
            Append (From_Stmt, ", user_post");
         end if;

         if Forum /= Forum_All then
            Append (From_Stmt, ", forum");
         end if;

         return To_String (From_Stmt);
      end Build_From;

      ------------------
      -- Build_Select --
      ------------------

      function Build_Select
        (Count_Only : in Boolean := False) return String
      is
         Select_Stmt : Unbounded_String;
      begin
         if Count_Only then
            Select_Stmt := +"SELECT COUNT(post.id)";

         else
            case Mode is
               when Everything =>
                  Select_Stmt := +"SELECT post.id, post.name, "
                    & Timezone.Date_Time ("post.date_post", TZ) & ", "
                    & "DATETIME(date_post, '+"
                    & Utils.Image (Settings.Anonymity_Hours)
                    & " hour') < DATETIME('NOW'), "
                    & "(SELECT filename FROM photo WHERE id=post.photo_id), "
                    & "category.name, comment_counter,"
                    & "visit_counter, post.hidden, user_post.user_login, "
                    & Timezone.Date_Time ("post.last_activity", TZ) & ", "
                    & "(SELECT id FROM photo_of_the_week "
                    & "WHERE post.id = photo_of_the_week.post_id)";

                  if Login /= "" then
                     --  Is_New? Check last activity against last user visit
                     --  but only if post.date less than 30 day old. Note that
                     --  we are using <= as the last visit date is set after we
                     --  retreive data for the post. So we really want to catch
                     --  posts done at the same time we have gone into the
                     --  forum.

                     Append
                       (Select_Stmt,
                        ", (post.last_activity>DATE ('now', '-30 days')"
                        & "   AND (COALESCE ((SELECT v.last_activity"
                        & " FROM last_user_visit v"
                        & " WHERE v.post_id=post.id"
                        & "   AND v.user_login=" & Q (Login) & "), "
                        & " DATE('now','-30 days')) < post.last_activity)) ");
                  end if;

               when Navigation_Only =>
                  Select_Stmt := +"SELECT post.id";
            end case;

            case Sorting is
               when Last_Posted | Need_Attention | Last_Commented =>
                  null;

               when Best_Noted =>
                  Append
                    (Select_Stmt,
                     ", (SELECT SUM(global_rating.post_rating)"
                       & " FROM global_rating"
                       & " WHERE post.id=global_rating.post_id)"
                       & " AS sum_rating");
            end case;

         end if;

         return -Select_Stmt;
      end Build_Select;

      -----------------
      -- Build_Where --
      -----------------

      function Build_Where
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Forum      : in Forum_Filter;
         Count_Only : in Boolean := False) return String
      is
         Where_Stmt : Unbounded_String :=
                        +" WHERE post.category_id=category.id";
      begin
         if not Count_Only or else User /= "" then
            --  if count_only is false, join with user_post table
            --  as we want to display the user_name
            --  if count_only is true and user is not null then
            --  join in needed to restrict to the given user
            Append (Where_Stmt, " AND user_post.post_id=post.id");
         end if;

         if Fid /= Empty_Id then
            --  Restrict query to the given forum id
            Append (Where_Stmt,
                    " AND category.forum_id=" & To_String (Fid));
         end if;

         if User /= "" then
            --  Restrict to a specific user
            Append (Where_Stmt,
                    " AND user_post.user_login=" & Q (User));
         end if;

         if Filter_Cat /= "" then
            Append (Where_Stmt,
                    " AND category.id=" & Q (Filter_Cat));
         end if;

         --  Sorting and filters

         case Sorting is
            when Last_Commented =>
               case Filter is
                  when Today .. One_Month =>
                     Append (Where_Stmt, " AND DATE(last_activity)");

                  when All_Messages =>
                     null;
               end case;

            when Last_Posted | Best_Noted | Need_Attention =>
               case Filter is
                  when Today .. One_Month =>
                     Append (Where_Stmt, " AND DATE(post.date_post)");

                  when All_Messages =>
                     null;
               end case;
         end case;

         case Filter is
            when Today =>
               Append (Where_Stmt, " =DATE(current_date)");

            when Two_Days =>
               Append (Where_Stmt, " >DATE(current_date, '-2 days')");

            when Seven_Days =>
               Append (Where_Stmt, " >DATE(current_date, '-7 days')");

            when Fifteen_Days =>
               Append (Where_Stmt, " >DATE(current_date, '-15 days')");

            when One_Month =>
               Append (Where_Stmt, " >DATE(current_date, '-1 month')");

            when All_Messages =>
               null;
         end case;

         case Forum is
            when Forum_Photo =>
               Append
                 (Where_Stmt,
                  " AND forum.for_photo='TRUE' "
                  &  " AND forum.id=category.forum_id");

            when Forum_Text =>
               Append
                 (Where_Stmt,
                  " AND forum.for_photo='FALSE' "
                  &  " AND forum.id=category.forum_id");

            when Forum_All =>
               null;
         end case;

         if Only_Revealed then
            Append
              (Where_Stmt,
               " AND (DATETIME(post.date_post, '+"
               & Utils.Image (V2P.Settings.Anonymity_Hours)
               & " hour')<DATETIME('NOW') OR forum.anonymity='FALSE') ");
         end if;

         if not Admin then
            Append (Where_Stmt, " AND post.hidden='FALSE'");
         end if;

         return -Where_Stmt;
      end Build_Where;

      -------------------
      -- Count_Threads --
      -------------------

      function Count_Threads
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Forum      : in Forum_Filter) return Natural
      is
         DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
         Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
         Line : DB.String_Vectors.Vector;
      begin
         Connect (DBH);

         DBH.Handle.Prepare_Select
           (Iter, Build_Select (Count_Only => True)
            & Build_From (User => User, Forum => Forum, Count_Only => True)
            & Build_Where
              (Fid        => Fid,
               User       => User,
               Admin      => Admin,
               Filter     => Filter,
               Filter_Cat => Filter_Cat,
               Forum      => Forum,
               Count_Only => True));

         if Iter.More then
            Iter.Get_Line (Line);
            Return_Count : declare
               Count : constant Natural :=
                         Natural'Value (DB.String_Vectors.Element (Line, 1));
            begin
               Line.Clear;
               Iter.End_Select;
               return Count;
            end Return_Count;
         end if;

         Line.Clear;
         Iter.End_Select;
         return 0;
      end Count_Threads;

      ----------------------------
      -- Threads_Ordered_Select --
      ----------------------------

      function Threads_Ordered_Select
        (Fid        : in Id;
         User       : in String;
         Admin      : in Boolean;
         From       : in Positive;
         Filter     : in Filter_Mode;
         Filter_Cat : in String;
         Order_Dir  : in Order_Direction;
         Limit      : in Natural;
         Forum      : in Forum_Filter) return Unbounded_String
      is
         SQL_Select  : constant String := Build_Select;
         SQL_From    : constant String :=
                         Build_From (User => User, Forum => Forum);
         SQL_Where   : constant String :=
                         Build_Where
                           (Fid        => Fid,
                            User       => User,
                            Admin      => Admin,
                            Filter     => Filter,
                            Filter_Cat => Filter_Cat,
                            Forum      => Forum);
         Select_Stmt : Unbounded_String :=
                         +SQL_Select & SQL_From & SQL_Where;
      begin
         --  Add filtering into the select statement

         case Sorting is
            when Last_Posted =>
               Append (Select_Stmt, " ORDER BY post.date_post");
               Append (Select_Stmt, ' ' & Order_Direction'Image (Order_Dir));

            when Last_Commented =>
               Append (Select_Stmt, " ORDER BY post.last_activity");
               Append (Select_Stmt, ' ' & Order_Direction'Image (Order_Dir));
               Append (Select_Stmt, ", post.date_post");
               Append (Select_Stmt, ' ' & Order_Direction'Image (Order_Dir));

            when Best_Noted =>
               Append (Select_Stmt, " ORDER BY sum_rating");
               Append (Select_Stmt, ' ' & Order_Direction'Image (Order_Dir));

            when Need_Attention =>
               --  No comment and oldest first
               Append (Select_Stmt, " ORDER BY post.comment_counter ASC");
               Append (Select_Stmt, ", post.date_post ASC");
         end case;

         if Limit /= 0 then
            Append (Select_Stmt,
                    " LIMIT " & Utils.Image (Limit)
                    & " OFFSET " & Utils.Image (From - 1));
         end if;

         return Select_Stmt;
      end Threads_Ordered_Select;

      DBH             : constant TLS_DBH_Access :=
                          TLS_DBH_Access (DBH_TLS.Reference);
      Iter            : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line            : DB.String_Vectors.Vector;
      Id              : Templates.Tag;
      Name            : Templates.Tag;
      Date_Post       : Templates.Tag;
      Revealed        : Templates.Tag;
      Category        : Templates.Tag;
      Comment_Counter : Templates.Tag;
      Visit_Counter   : Templates.Tag;
      Thumb           : Templates.Tag;
      Hidden          : Templates.Tag;
      Owner           : Templates.Tag;
      Date_Last_Com   : Templates.Tag;
      Is_CDC          : Templates.Tag;
      Is_New          : Templates.Tag;
      Select_Stmt     : Unbounded_String;

   begin
      Total_Lines := Count_Threads
        (Fid        => Fid,
         User       => User,
         Admin      => Admin,
         Filter     => Filter,
         Filter_Cat => Filter_Cat,
         Forum      => Forum);

      if Total_Lines = 0 then
         --  Nothing to print. Avoid to return an empty page.
         --  Insert a tag to display a message to the user telling him
         --  that the requested search fail and has been replaced by another
         --  filter.
         Templates.Insert
           (Set, Templates.Assoc
              (Block_Forum_Threads.NEW_FILTER, "NEW FILTER"));

         if Filter /= All_Messages then
            Restart_With_New_Filter : declare
               New_Filter : constant Filter_Mode := Filter_Mode'Succ (Filter);
            begin
               Get_Threads
                 (Fid, Login, User, Admin, Forum, Page_Size, New_Filter,
                  Filter_Cat, Order_Dir, Sorting, Only_Revealed, From,
                  Mode, Navigation, Set, Nb_Lines, Total_Lines, TZ);
               return;
            end Restart_With_New_Filter;
         end if;
      end if;

      if Total_Lines < From then
         From := 1; -- ??? What should be done in this case ?
      end if;

      Navigation := V2P.Navigation_Links.Post_Ids.Empty_Vector;

      Connect (DBH);

      Select_Stmt := Threads_Ordered_Select
        (Fid        => Fid,
         User       => User,
         Admin      => Admin,
         From       => From,
         Limit      => Page_Size,
         Filter     => Filter,
         Filter_Cat => Filter_Cat,
         Order_Dir  => Order_Dir,
         Forum      => Forum);

      DBH.Handle.Prepare_Select (Iter, To_String (Select_Stmt));

      Nb_Lines := 0;

      while Iter.More loop
         Iter.Get_Line (Line);
         Nb_Lines := Nb_Lines + 1; --  ??? Maybe a smarter way to do this

         if Mode = Everything then
            Id              := Id        & DB.String_Vectors.Element (Line, 1);
            Name            := Name      & DB.String_Vectors.Element (Line, 2);
            Date_Post       := Date_Post & DB.String_Vectors.Element (Line, 3);
            Revealed        := Revealed  & DB.String_Vectors.Element (Line, 4);
            Thumb           := Thumb     & DB.String_Vectors.Element (Line, 5);
            Category        := Category  & DB.String_Vectors.Element (Line, 6);
            Comment_Counter := Comment_Counter
              & DB.String_Vectors.Element (Line, 7);
            Visit_Counter   := Visit_Counter
              & DB.String_Vectors.Element (Line, 8);
            Hidden          := Hidden    & DB.String_Vectors.Element (Line, 9);
            Owner           := Owner
              & DB.String_Vectors.Element (Line, 10);
            Date_Last_Com   := Date_Last_Com
              & DB.String_Vectors.Element (Line, 11);
            Is_CDC          := Is_CDC
              & (DB.String_Vectors.Element (Line, 12) /= "");

            if Login /= ""  then
               Is_New := Is_New
                 & (DB.String_Vectors.Element (Line, 13) /= "0");
            end if;
         end if;

         --  Insert this post id in navigation links

         Navigation := Navigation & Database.Id'Value
           (DB.String_Vectors.Element (Line, 1));

         Line.Clear;
      end loop;

      Iter.End_Select;

      if Mode = Everything then
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.THUMB_SOURCE, Thumb));

         Templates.Insert (Set, Templates.Assoc (Chunk_Threads_List.TID, Id));
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.NAME, Name));
         Templates.Insert
           (Set, Templates.Assoc
              (Chunk_Threads_Text_List.DATE_POST, Date_Post));
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.CATEGORY, Category));
         Templates.Insert
           (Set, Templates.Assoc
              (Chunk_Threads_List.COMMENT_COUNTER, Comment_Counter));
         Templates.Insert
           (Set, Templates.Assoc
              (Chunk_Threads_List.VISIT_COUNTER, Visit_Counter));
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.REVEALED, Revealed));
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.OWNER, Owner));
         Templates.Insert
           (Set, Templates.
              Assoc (Chunk_Threads_List.DATE_LAST_COMMENT,
                Date_Last_Com));
         Templates.Insert
           (Set, Templates.Assoc (Chunk_Threads_List.HIDDEN, Hidden));
         Templates.Insert
           (Set, Templates.Assoc
              (Chunk_List_Navlink.NAV_NB_LINES_TOTAL, Total_Lines));
         Templates.Insert
           (Set, Templates.Assoc
              (Chunk_List_Navlink.NB_LINE_RETURNED, Nb_Lines));
         Templates.Insert
           (Set, Templates.Assoc
              (Block_User_Photo_List.IS_CDC, Is_CDC));

         if Login /= "" then
            Templates.Insert
              (Set, Templates.Assoc (Chunk_Threads_List.IS_NEW, Is_New));
         end if;
      end if;

      Templates.Insert
        (Set, Templates.Assoc (Set_Global.NAV_FROM, From));
   end Get_Threads;

   -------------------
   -- Get_Thumbnail --
   -------------------

   function Get_Thumbnail (Post : in Id) return String is
      DBH      : constant TLS_DBH_Access :=
                   TLS_DBH_Access (DBH_TLS.Reference);
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      Filename : Unbounded_String;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT filename FROM photo, post "
         & "WHERE photo.id=post.photo_id AND post.id=" & To_String (Post));

      if Iter.More then
         Iter.Get_Line (Line);

         Filename := To_Unbounded_String (DB.String_Vectors.Element (Line, 1));

         Line.Clear;
      end if;

      Iter.End_Select;

      return To_String (Filename);
   end Get_Thumbnail;

   ----------------------
   -- Get_User_Comment --
   ----------------------

   function Get_User_Comment
     (Uid     : in String;
      Limit   : in Positive;
      Textify : in Boolean := False) return Templates.Translate_Set
   is
      SQL        : constant String :=
                     "SELECT c.id, c.comment, "
                       & "(SELECT pc.post_id"
                       & " FROM post_comment AS pc, post AS p,"
                       & " user_post AS u"
                       & " WHERE pc.comment_id=c.id AND p.id=pc.post_id"
                       & " AND u.post_id=p.id"
                       --  Either the author is revealed or it is not Uid post
                       & " AND (DATETIME(p.date_post, '+"
                       & Utils.Image (V2P.Settings.Anonymity_Hours)
                       & " hour')<DATETIME('NOW') "
                       & " OR u.user_login!=" & Q (Uid) & ")) AS pid "
                       & "FROM comment AS c WHERE c.user_login=" & Q (Uid)
                       & " AND c.has_voted='FALSE' AND pid!='' "
                       & "ORDER BY c.id DESC LIMIT " & Utils.Image (Limit);
      DBH        : constant TLS_DBH_Access :=
                     TLS_DBH_Access (DBH_TLS.Reference);
      Set        : Templates.Translate_Set;
      Iter       : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line       : DB.String_Vectors.Vector;

      Post_Id    : Templates.Tag;
      Comment_Id : Templates.Tag;
      Comment    : Templates.Tag;

      use type Templates.Tag;

   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);
         Comment_Id := Comment_Id & DB.String_Vectors.Element (Line, 1);
         if Textify then
            Comment := Comment
              & Morzhol.Strings.HTML_To_Text
              (DB.String_Vectors.Element (Line, 2));
         else
            Comment := Comment & DB.String_Vectors.Element (Line, 2);
         end if;
         Post_Id    := Post_Id & DB.String_Vectors.Element (Line, 3);
         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Block_User_Comment_List.COMMENT_TID, Post_Id));
      Templates.Insert
        (Set,
         Templates.Assoc (Block_User_Comment_List.COMMENT_ID, Comment_Id));
      Templates.Insert
        (Set, Templates.Assoc (Block_User_Comment_List.COMMENT, Comment));

      return Set;
   end Get_User_Comment;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data (Uid : in String) return User_Data is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      if Uid = "" then
         return No_User_Data;
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT password, admin, email FROM user WHERE login=" & Q (Uid));

      if Iter.More then
         Iter.Get_Line (Line);

         User : declare
            Password : constant String := DB.String_Vectors.Element (Line, 1);
            Admin    : constant String := DB.String_Vectors.Element (Line, 2);
            Email    : constant String := DB.String_Vectors.Element (Line, 3);
            Prefs    : User_Settings;
         begin
            Line.Clear;
            Preference.User (Uid, Prefs);

            return User_Data'(UID         => +Uid,
                              Password    => +Password,
                              Admin       => Boolean'Value (Admin),
                              Email       => +Email,
                              Preferences => Prefs);
         end User;

      else
         return No_User_Data;
      end if;
   end Get_User_Data;

   ------------------------------
   -- Get_User_Data_From_Email --
   ------------------------------

   function Get_User_Data_From_Email (Email : in String) return User_Data is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      if Email = "" then
         return No_User_Data;
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT login, password, admin FROM user WHERE email=" & Q (Email));

      if Iter.More then
         Iter.Get_Line (Line);

         User : declare
            Login    : constant String := DB.String_Vectors.Element (Line, 1);
            Password : constant String := DB.String_Vectors.Element (Line, 2);
            Admin    : constant String := DB.String_Vectors.Element (Line, 3);
            Prefs    : User_Settings;
         begin
            Line.Clear;
            Preference.User (Login, Prefs);

            return User_Data'(UID         => +Login,
                              Password    => +Password,
                              Admin       => Boolean'Value (Admin),
                              Email       => +Email,
                              Preferences => Prefs);
         end User;

      else
         return No_User_Data;
      end if;
   end Get_User_Data_From_Email;

   -------------------------
   -- Get_User_Last_Photo --
   -------------------------

   function Get_User_Last_Photo
     (Uid : in String) return Templates.Translate_Set
   is
      DBH  : constant TLS_DBH_Access :=
               TLS_DBH_Access (DBH_TLS.Reference);
      SQL  : constant String :=
               "SELECT q.photo_id, p.filename "
                 & "FROM user_photo_queue q, photo p "
                 & "WHERE q.photo_id=p.id "
                 & "AND user_login=" & Q (Uid);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Page_Forum_New_Photo_Entry.PID,
               DB.String_Vectors.Element (Line, 1)));
         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Page_Forum_New_Photo_Entry.IMAGE_SOURCE,
              DB.String_Vectors.Element (Line, 2)));
         Line.Clear;
      end if;

      Iter.End_Select;
      return Set;
   end Get_User_Last_Photo;

   -------------------
   -- Get_User_Page --
   -------------------

   function Get_User_Page (Uid : in String) return Templates.Translate_Set is
      SQL          : constant String :=
                       "SELECT content, content_html FROM user_page "
                         & "WHERE user_login=" & Q (Uid);
      DBH          : constant TLS_DBH_Access :=
                       TLS_DBH_Access (DBH_TLS.Reference);
      Set          : Templates.Translate_Set;
      Iter         : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line         : DB.String_Vectors.Vector;
      Content      : Templates.Tag;
      Content_HTML : Templates.Tag;

      use type Templates.Tag;

   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         while Iter.More loop
            Iter.Get_Line (Line);
            Content      := Content & DB.String_Vectors.Element (Line, 1);
            Content_HTML := Content_HTML & DB.String_Vectors.Element (Line, 2);

            Line.Clear;
         end loop;

         Iter.End_Select;

         Templates.Insert
           (Set, Templates.Assoc (Block_User_Page.USER_PAGE_CONTENT, Content));
         Templates.Insert
           (Set, Templates.Assoc
              (Block_User_Page.USER_PAGE_HTML_CONTENT, Content_HTML));

      else
         Templates.Insert
           (Set, Templates.Assoc (Block_User_Page.USER_NOT_FOUND, True));
      end if;

      return Set;
   end Get_User_Page;

   --------------------
   -- Get_User_Stats --
   --------------------

   function Get_User_Stats (Uid, TZ : in String) return User_Stats is
      use type AWS.Templates.Tag;

      SQL     : constant String :=
                  "SELECT login, " & Timezone.Date ("created", TZ)
                  & ", " & Timezone.Date ("last_logged", TZ) & ", "
                  & "nb_com, "
                  --  nb photos, do not use the data from user's stats table
                  --  has the non revealed photos are counted in this table.
                  --  This is not very important in the table listing all users
                  --  but on the user page it is because from this page it is
                  --  easy to know how many photos not yet revealed the author
                  --  has posted.
                  & "(SELECT count (post_id) FROM post, user_post,"
                  & " forum, category"
                  & " WHERE post.id=post_id AND post.photo_id!=0"
                  & " AND user_post.user_login=user.login"
                  & " AND post.category_id=category.id"
                  & " AND forum.id=category.forum_id"
                  & " AND post.hidden='FALSE'"
                  & " AND (DATETIME(post.date_post, '+"
                  & Utils.Image (V2P.Settings.Anonymity_Hours)
                  & " hour')<DATETIME('NOW') OR forum.anonymity='FALSE')), "
                  & "nb_mess, nb_cdc "
                  & "FROM user, user_stats "
                  & "WHERE user.login=" & Q (Uid)
                  & " AND user.login=user_stats.user_login";
      DBH    : constant TLS_DBH_Access :=
                 TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line   : DB.String_Vectors.Vector;
      Result : User_Stats;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         Result.Created        := +DB.String_Vectors.Element (Line, 2);
         Result.Last_Connected := +DB.String_Vectors.Element (Line, 3);
         Result.N_Comments :=
           Natural'Value (DB.String_Vectors.Element (Line, 4));
         Result.N_Photos :=
           Natural'Value (DB.String_Vectors.Element (Line, 5));
         Result.N_Messages :=
           Natural'Value (DB.String_Vectors.Element (Line, 6));
         Result.N_CdC :=
           Natural'Value (DB.String_Vectors.Element (Line, 7));
      end if;

      Iter.End_Select;

      return Result;
   end Get_User_Stats;

   function Get_User_Stats
     (Uid, TZ : in String) return Templates.Translate_Set
   is
      use type AWS.Templates.Tag;

      Stats : constant User_Stats := Get_User_Stats (Uid, TZ);
      Set   : Templates.Translate_Set;
   begin
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.N_PHOTOS, Stats.N_Photos));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.N_MESSAGES, Stats.N_Messages));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.N_COMMENTS, Stats.N_Comments));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.N_CDC, Stats.N_CdC));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.REGISTERED_DATE, Stats.Created));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Stats.LAST_CONNECTED_DATE,
            Stats.Last_Connected));
      return Set;
   end Get_User_Stats;

   ---------------
   -- Get_Users --
   ---------------

   function Get_Users
     (From  : in Positive;
      Sort  : in User_Sort;
      Order : in Order_Direction;
      TZ    : in String) return Templates.Translate_Set
   is
      use type AWS.Templates.Tag;

      function Sort_Order return String;
      --  Returns the proper SQL order statement

      ----------------
      -- Sort_Order --
      ----------------

      function Sort_Order return String is
         Result : Unbounded_String := +"ORDER BY ";
      begin
         case Sort is
            when Date_Created =>
               Append (Result, "created");
            when Last_Connected =>
               Append (Result, "last_logged");
            when  Nb_Comments =>
               Append (Result, "nb_com");
            when Nb_Photos =>
               Append (Result, "nb_photo");
            when Nb_CdC =>
               Append (Result, "nb_cdc");
         end case;

         Append (Result, " " & Order_Direction'Image (Order));
         return -Result;
      end Sort_Order;

      DBH             : constant TLS_DBH_Access :=
                          TLS_DBH_Access (DBH_TLS.Reference);
      SQL             : constant String :=
                          "SELECT login, " & Timezone.Date ("created", TZ)
                            & ", " & Timezone.Date ("last_logged", TZ) & ", "
                            & "nb_com, nb_photo, nb_cdc "
                            & "FROM user, user_stats "
                            & "WHERE user.login=user_stats.user_login "
                            & Sort_Order & " LIMIT"
                            & Positive'Image (Settings.Number_Users_Listed)
                            & " OFFSET" & Positive'Image (From - 1);
      Set             : Templates.Translate_Set;
      Iter            : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line            : DB.String_Vectors.Vector;
      Login           : Templates.Tag;
      Registered_Date : Templates.Tag;
      L_Connect_Date  : Templates.Tag;
      Nb_Comments     : Templates.Tag;
      Nb_Photos       : Templates.Tag;
      Nb_CdC          : Templates.Tag;
      Lines           : Natural := 0;
   begin
      Connect (DBH);

      --  Count nb results

      declare
         SQL : constant String := "SELECT count(*) from user";
      begin
         DBH.Handle.Prepare_Select (Iter, SQL);
         if Iter.More then
            Iter.Get_Line (Line);

            Templates.Insert
              (Set,
               Templates.Assoc
                 (Set_Global.NAV_NB_LINES_TOTAL,
                  DB.String_Vectors.Element (Line, 1)));
         end if;
         Line.Clear;
      end;

      Templates.Insert (Set, Templates.Assoc (Set_Global.NAV_FROM, From));

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);
         Lines := Lines + 1;

         Login := Login & DB.String_Vectors.Element (Line, 1);
         Registered_Date :=
           Registered_Date & DB.String_Vectors.Element (Line, 2);
         L_Connect_Date :=
           L_Connect_Date & DB.String_Vectors.Element (Line, 3);
         Nb_Comments := Nb_Comments & DB.String_Vectors.Element (Line, 4);
         Nb_Photos := Nb_Photos & DB.String_Vectors.Element (Line, 5);
         Nb_CdC := Nb_CdC & DB.String_Vectors.Element (Line, 6);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_List_Navlink.NB_LINE_RETURNED, Lines));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.LOGIN, Login));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.REGISTERED_DATE, Registered_Date));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.LAST_CONNECTED_DATE, L_Connect_Date));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.N_PHOTOS, Nb_Photos));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.N_COMMENTS, Nb_Comments));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_Users.N_CDC, Nb_CdC));

      return Set;
   end Get_Users;

   -----------------------------
   -- Increment_Visit_Counter --
   -----------------------------

   procedure Increment_Visit_Counter (Pid : in Id) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "UPDATE post SET visit_counter=visit_counter+1"
                & " WHERE id=" & To_String (Pid);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Increment_Visit_Counter;

   --------------------
   -- Insert_Comment --
   --------------------

   function Insert_Comment
     (Uid       : in String;
      Anonymous : in String;
      Thread    : in Id;
      Name      : in String;
      Comment   : in String;
      Pid       : in Id) return Id
   is
      pragma Unreferenced (Name);

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String);
      --  Insert row into Comment table

      procedure Insert_Table_Post_Comment (Post_Id, Comment_Id : in Id);
      --  Insert row into post_Comment table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      --------------------------
      -- Insert_Table_Comment --
      --------------------------

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String)
      is
         SQL : constant String :=
                 "INSERT INTO comment ('user_login', 'anonymous_user', "
                   & "'comment', 'photo_id')"
                   & " VALUES ("
                   & Q (User_Login) & ',' & Q (Anonymous) & ',' & Q (Comment)
                   & ',' & To_String (Pid) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Comment;

      --------------------------------
      -- Insert_Table_post_Comment --
      --------------------------------

      procedure Insert_Table_Post_Comment (Post_Id, Comment_Id : in Id) is
         SQL : constant String :=
                 "INSERT INTO post_comment VALUES ("
                   & To_String (Post_Id) & "," & To_String (Comment_Id) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Post_Comment;

   begin
      Connect (DBH);
      DBH.Handle.Begin_Transaction;
      Insert_Table_Comment (Uid, Anonymous, Comment);

      Row_Id : declare
         Cid : constant Id := Id'Value (DBH.Handle.Last_Insert_Rowid);
      begin
         Insert_Table_Post_Comment (Thread, Cid);
         DBH.Handle.Commit;
         return Cid;
      end Row_Id;
   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Logs.Write
            (Name    => Module,
             Kind    => Logs.Error,
             Content => Exception_Message (E));
         return Empty_Id;
   end Insert_Comment;

   ---------------------
   -- Insert_Metadata --
   ---------------------

   procedure Insert_Metadata
     (Pid                     : in Id;
      Geo_Latitude            : in Float;
      Geo_Longitude           : in Float;
      Geo_Latitude_Formatted  : in String;
      Geo_Longitude_Formatted : in String)
   is
      SQL : constant String := "INSERT INTO photo_metadata (photo_id, "
        & "geo_latitude, geo_longitude, geo_latitude_formatted, "
        & "geo_longitude_formatted) VALUES ("
        & "(SELECT photo_id FROM post WHERE id=" & To_String (Pid) & "), "
        & F (Geo_Latitude) & ", " & F (Geo_Longitude) & ", "
        & Q (Geo_Latitude_Formatted) & ", "
        & Q (Geo_Longitude_Formatted) & ")";

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Write
            (Name    => Module,
             Kind    => Logs.Error,
             Content => Exception_Message (E));
   end Insert_Metadata;

   ------------------
   -- Insert_Photo --
   ------------------

   function Insert_Photo
     (Uid           : in String;
      Filename      : in String;
      Height        : in Integer;
      Width         : in Integer;
      Medium_Height : in Integer;
      Medium_Width  : in Integer;
      Thumb_Height  : in Integer;
      Thumb_Width   : in Integer;
      Size          : in Integer) return String
   is

      procedure Insert_Table_Photo
        (Filename      : in String;
         Height        : in Integer;
         Width         : in Integer;
         Medium_Height : in Integer;
         Medium_Width  : in Integer;
         Thumb_Height  : in Integer;
         Thumb_Width   : in Integer;
         Size          : in Integer);
      --  Insert row into the photo table

      procedure User_Tmp_Photo (Uid, Pid : in String);
      --  Update user_photo_queue table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      ------------------------
      -- Insert_Table_Photo --
      ------------------------

      procedure Insert_Table_Photo
        (Filename      : in String;
         Height        : in Integer;
         Width         : in Integer;
         Medium_Height : in Integer;
         Medium_Width  : in Integer;
         Thumb_Height  : in Integer;
         Thumb_Width   : in Integer;
         Size          : in Integer)
      is
         SQL : constant String :=
                 "INSERT INTO photo ('filename', 'height', 'width', "
                  & "'medium_height', 'medium_width', "
                  & "'thumb_height', 'thumb_width', 'size') "
                  & "VALUES (" & Q (Filename) & ','
                  & I (Height) & ',' & I (Width) & ','
                  & I (Medium_Height) & ',' & I (Medium_Width) & ','
                  & I (Thumb_Height) & ',' & I (Thumb_Width) & ','
                  & I (Size) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Photo;

      --------------------
      -- User_Tmp_Photo --
      --------------------

      procedure User_Tmp_Photo (Uid, Pid : in String) is
         SQL : constant String :=
                 "UPDATE user_photo_queue SET photo_id=" & Pid
                   & " WHERE user_login=" & Q (Uid);
      begin
         DBH.Handle.Execute (SQL);
      end User_Tmp_Photo;

   begin
      Connect (DBH);

      DBH.Handle.Begin_Transaction;

      Insert_Table_Photo (Filename, Height, Width, Medium_Height, Medium_Width,
                          Thumb_Height, Thumb_Width, Size);

      Row_Id : declare
         Pid : constant String := DBH.Handle.Last_Insert_Rowid;
      begin
         User_Tmp_Photo (Uid, Pid);
         DBH.Handle.Commit;
         return Pid;
      end Row_Id;
   exception
      when others =>
         DBH.Handle.Rollback;
         return "";
   end Insert_Photo;

   -----------------
   -- Insert_Post --
   -----------------

   function Insert_Post
     (Uid         : in String;
      Category_Id : in Id;
      Name        : in String;
      Comment     : in String;
      Pid         : in Id) return Id
   is
      procedure Insert_Table_Post
        (Name, Category_Id, Comment, Photo_Id : in String);
      --  Insert row into the post table

      procedure Insert_Table_User_Post (Uid : in String; Post_Id : in Id);
      --  Insert row into the user_post table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      ------------------------
      -- Insert_Table_post --
      ------------------------

      procedure Insert_Table_Post
        (Name, Category_Id, Comment, Photo_Id : in String)
      is
         SQL : constant String :=
                 "INSERT INTO post ('name', 'comment', 'category_id',"
                   & "'template_id', 'visit_counter', 'comment_counter',"
                   & "'photo_id') VALUES (" & Q (Name) &  ',' & Q (Comment)
                   & ',' & Category_Id & ", 1, 0, 0," & Photo_Id & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Post;

      -----------------------------
      -- Insert_Table_User_post --
      -----------------------------

      procedure Insert_Table_User_Post (Uid : in String; Post_Id : in Id) is
         SQL : constant String :=
                 "INSERT INTO user_post VALUES ("
                   & Q (Uid) & ',' & To_String (Post_Id) & ")";
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_User_Post;

   begin
      Connect (DBH);

      DBH.Handle.Begin_Transaction;

      if Pid /= Empty_Id then
         Insert_Table_Post
           (Name, To_String (Category_Id), Comment, To_String (Pid));
      else
         Insert_Table_Post (Name, To_String (Category_Id), Comment, "NULL");
      end if;

      Row_Id : declare
         Post_Id : constant Id := Id'Value (DBH.Handle.Last_Insert_Rowid);
      begin
         Insert_Table_User_Post (Uid, Post_Id);
         DBH.Handle.Commit;

         --  Set the last forum visit for the posting user. We do not want to
         --  have this forum appearing as containing new message for the
         --  author of the post.

         Set_Last_Forum_Visit (Uid, Get_Fid_From_Category (DBH, Category_Id));

         return Post_Id;
      end Row_Id;

   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Logs.Write
            (Name    => Module,
             Kind    => Logs.Error,
             Content => Exception_Message (E));
         return Empty_Id;
   end Insert_Post;

   ---------------
   -- Is_Author --
   ---------------

   function Is_Author (Uid : in String; Pid : in Id) return Boolean is
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Result : Boolean := False;
   begin
      Connect (DBH);

      --  Get post Pid posted by user Uid

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT * FROM user_post WHERE post_id="
           & To_String (Pid) & " AND user_login=" & Q (Uid));

      if Iter.More then
         Result := True;
      end if;

      Iter.End_Select;

      return Result;
   end Is_Author;

   -----------------
   -- Is_Revealed --
   -----------------

   function Is_Revealed (Tid : in Id) return Boolean is
      DBH    : constant TLS_DBH_Access :=
                 TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line   : DB.String_Vectors.Vector;
      Result : Boolean;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT "
         & " (JULIANDAY(post.date_post, '+"
         & Utils.Image (Settings.Anonymity_Hours)
         & " hour') - JULIANDAY('NOW')) * 24 "
         & "FROM post WHERE post.id=" & To_String (Tid));

      Result := False;

      if Iter.More then
         Iter.Get_Line (Line);

         Is_Revealed : declare
            Hours : constant Float :=
                      Float'Value (DB.String_Vectors.Element (Line, 1));
         begin
            if Hours < 0.0 then
               Result := True;
            end if;
         end Is_Revealed;

         Iter.End_Select;
      end if;

      return Result;
   end Is_Revealed;

   ------------------
   -- Set_Category --
   ------------------

   procedure Set_Category (Tid : in Id; Category_Id : in Id) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "UPDATE post SET category_id=" & To_String (Category_Id)
              & " WHERE post.id=" & To_String (Tid);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Set_Category;

   --------------------------
   -- Set_Last_Forum_Visit --
   --------------------------

   procedure Set_Last_Forum_Visit (Login : in String; FID : in Id) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "INSERT OR REPLACE INTO last_forum_visit "
                & "('user_login', 'forum_id', 'last_post_id') VALUES ("
                & Q (Login) & ", " & I (FID)
                & ", (SELECT post.id FROM post, category WHERE "
                & "post.category_id=category.id AND category.forum_id="
                & I (FID) & " ORDER BY post.id DESC LIMIT 1))";
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Set_Last_Forum_Visit;

   ---------------------
   -- Set_Last_Logged --
   ---------------------

   procedure Set_Last_Logged (Uid : in String) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute
        ("UPDATE user SET last_logged=DATETIME('NOW') WHERE login=" & Q (Uid));
   end Set_Last_Logged;

   --------------------
   -- Set_Last_Visit --
   --------------------

   procedure Set_Last_Visit (Login : in String; TID : in Id) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "INSERT OR REPLACE INTO last_user_visit "
                & "('user_login', 'post_id', 'last_activity') VALUES ("
                & Q (Login) & ", " & I (TID) & ", current_timestamp)";
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Set_Last_Visit;

   ---------------
   -- To_String --
   ---------------

   function To_String (Id : in Database.Id) return String is
   begin
      return Utils.Image (Id);
   end To_String;

   --------------------------
   -- Toggle_Hidden_Status --
   --------------------------

   function Toggle_Hidden_Status
     (Tid : in Id) return Templates.Translate_Set
   is
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line   : DB.String_Vectors.Vector;
      Hidden : Boolean := True;
      Set    : Templates.Translate_Set;
   begin
      Connect (DBH);

      --  Get current hidden status

      DBH.Handle.Prepare_Select
        (Iter, "SELECT hidden FROM post WHERE post.id=" & To_String (Tid));

      if Iter.More then
         Iter.Get_Line (Line);
         Hidden := Boolean'Value (DB.String_Vectors.Element (Line, 1));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Toggle and store new status

      Hidden := not Hidden;

      DBH.Handle.Execute
        ("UPDATE post SET hidden="
         & Q (Hidden) & " WHERE id=" & To_String (Tid));

      Templates.Insert
        (Set, Templates.Assoc
           (Page_Forum_Entry.HIDDEN, Boolean'Image (Hidden)));
      return Set;

   exception
      when E : DB.DB_Error =>
         Logs.Write
            (Name    => Module,
             Kind    => Logs.Error,
             Content => Exception_Message (E));
         return Set;
   end Toggle_Hidden_Status;

   -----------------
   -- Update_Page --
   -----------------

   procedure Update_Page
     (Uid : in String; Content : in String; Content_HTML : in String)
   is
      SQL : constant String :=
              "UPDATE user_page SET content_html=" & Q (Content_HTML)
              & ", content=" & Q (Content) & " WHERE user_login=" & Q (Uid);

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Update_Page;

end V2P.Database;
