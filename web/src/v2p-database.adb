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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Task_Attributes;
with Ada.Text_IO;

with AWS.Utils;

with DB;
with Image.Metadata.Embedded;
with Morzhol.OS;
with Morzhol.Strings;

with V2P.DB_Handle;
with V2P.Logs;
with V2P.Settings;
with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Forum_New_Photo_Entry;
with V2P.Template_Defs.Chunk_Comment;
with V2P.Template_Defs.Block_Exif;
with V2P.Template_Defs.Block_Forum_Threads;
with V2P.Template_Defs.Block_Forum_Threads_Text;
with V2P.Template_Defs.Block_Forum_Navigate;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Latest_Posts;
with V2P.Template_Defs.Block_Latest_Users;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_Metadata;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Block_User_Comment_List;
with V2P.Template_Defs.Block_Global_Rating;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Set_Global;
with V2P.Template_Defs.R_Block_Forum_List;

package body V2P.Database is

   use Ada;
   use Ada.Exceptions;

   use Morzhol.OS;

   use V2P.Context;
   use V2P.Template_Defs;

   Module : constant Logs.Module_Name := "Database";

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   type TLS_DBH_Access is access all TLS_DBH;

   Null_DBH : constant TLS_DBH :=
                TLS_DBH'(Handle => null, Connected => False);

   package DBH_TLS is
     new Task_Attributes (Attribute => TLS_DBH, Initial_Value => Null_DBH);

   procedure Connect (DBH : in TLS_DBH_Access);
   --  Connect to the database if needed

   function F (F : in Float) return String;
   pragma Inline (F);
   --  Returns float image

   function I (Int : in Integer) return String;
   pragma Inline (I);
   --  Returns Integer image

   function Q (Str : in String) return String;
   pragma Inline (Q);
   --  Quote the string and double all single quote in Str to be able to insert
   --  a quote into the database.
   --  Returns Null if empty string

   function Q (Str : in Unbounded_String) return String;
   pragma Inline (Q);
   --  As above but from an Unbounded_String

   function Q (Bool : in Boolean) return String;
   pragma Inline (Q);
   --  As above but for a boolean

   function "+"
     (Str : in String) return Unbounded_String renames To_Unbounded_String;

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in TLS_DBH_Access) is
      DB_Path : constant String :=
                  Gwiad_Plugin_Path & Directory_Separator
                    & Settings.Get_DB_Name;
   begin
      if not DBH.Connected then
         if Directories.Exists (Name => DB_Path) then
            DBH.Handle := new DB.Handle'Class'(DB_Handle.Get);
            DBH.Handle.Connect (DB_Path);
            DBH.Connected := True;
            DBH_TLS.Set_Value (DBH.all);
         else
            Logs.Write
              (Module, Logs.Error, "ERROR : No database found : " & DB_Path);
            raise No_Database
              with "ERROR : No database found : " & DB_Path;
         end if;
      end if;
   end Connect;

   -------
   -- F --
   -------

   function F (F : in Float) return String is
   begin
      return Float'Image (F);
   end F;

   --------------------
   -- Get_Categories --
   --------------------

   function Get_Categories (Fid : in String) return Templates.Translate_Set is
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
        (Iter, "select id, name from category"
         & " where forum_id=" & Q (Fid));

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

   function Get_Category (Tid : in String) return Templates.Translate_Set is
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
        (Iter, "select id, name from category"
         & " where post.category_id=category.id post.id=" & Q (Tid));

      if Iter.More then
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end if;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Block_New_Comment.Category_Id, Id));
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
        (Iter, "select f.name, c.name from category c, "
         & "forum f where f.id = c.forum_id and c.id = " & Q (CID));

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

   function Get_Comment (Cid : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "select strftime('%Y-%m-%dT%H:%M:%SZ', date), "
         & "date(date, 'localtime'), time(date, 'localtime'), "
         & "user_login, anonymous_user, "
         & "comment"
         --  & "(select filename from photo where id=comment.photo_id) "
         --  ??? Filename is not used for now
         & " from comment where id=" & Cid);

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
         Line.Clear;
      end if;

      Iter.End_Select;
      return Set;
   end Get_Comment;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Tid        : in String;
      Forum_Type : in V2P.Database.Forum_Type)
      return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      Templates.Insert (Set, Get_Post (Tid, Forum_Type));

      Get_All_Comments : declare
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
      begin

         DBH.Handle.Prepare_Select
           (Iter,
            "select comment.id, strftime('%Y-%m-%dT%H:%M:%SZ', date), "
            & "date(date, 'localtime'), time(date, 'localtime'), "
            & "user_login, anonymous_user, "
            & "comment, "
            & "(select filename from photo where id=comment.photo_id) "
            & " from comment, post_comment"
            & " where post_id=" & Q (Tid)
            & " and post_comment.comment_id=comment.id");

         while Iter.More loop
            Iter.Get_Line (Line);

            Comment_Id    := Comment_Id
              & DB.String_Vectors.Element (Line, 1);
            Date_Iso_8601 := Date_Iso_8601
              & DB.String_Vectors.Element (Line, 2);
            Date          := Date
              & DB.String_Vectors.Element (Line, 3);
            Time          := Time
              & DB.String_Vectors.Element (Line, 4);
            User          := User
              & DB.String_Vectors.Element (Line, 5);
            Anonymous     := Anonymous
              & DB.String_Vectors.Element (Line, 6);
            Comment       := Comment
              & DB.String_Vectors.Element (Line, 7);
            Filename      := Filename
              & DB.String_Vectors.Element (Line, 8);

            --  Unthreaded view

            Comment_Level      := Comment_Level      & 1;
            Nb_Levels_To_Close := Nb_Levels_To_Close & 1;

            Line.Clear;
         end loop;

         Iter.End_Select;

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.COMMENT_ID, Comment_Id));
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
           (Set, Templates.Assoc
              (Template_Defs.Chunk_Comment.COMMENT, Comment));
         Templates.Insert
           (Set, Templates.Assoc
              (Page_Forum_Entry.COMMENT_LEVEL, Comment_Level));
         Templates.Insert
           (Set,
            Templates.Assoc
              (Page_Forum_Entry.NB_LEVELS_TO_CLOSE, Nb_Levels_To_Close));
      end Get_All_Comments;

      return Set;
   end Get_Entry;

   --------------
   -- Get_Exif --
   --------------

   function Get_Exif (Pid : in String) return Templates.Translate_Set is

      function "+"
        (Str : in String) return Unbounded_String renames To_Unbounded_String;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;

      Exif : Image.Metadata.Embedded.Data;

   begin
      if Pid = "" then
         --  ???
         return Set;
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select create_date, make, camera_model_name, "
         & "shutter_speed_value, aperture_value, flash, focal_length, "
         & "exposure_mode, exposure_program, white_balance, metering_mode, "
         & "iso from photo_exif "
         & "where photo_id = " & Q (Pid));

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
           (Iter, "select filename from photo where id=" & Q (Pid));

         if Iter.More then
            Iter.Get_Line (Line);

            Exif := Image.Metadata.Embedded.Get
              (Settings.Get_Images_Path & Directory_Separator
               & DB.String_Vectors.Element (Line, 1));
         end if;

         DBH.Handle.Execute
           ("insert into photo_exif " &
            "('photo_id', 'create_date', 'make', 'camera_model_name', "
            & "'shutter_speed_value', 'aperture_value', 'flash', "
            & "'focal_length', 'exposure_mode', 'exposure_program', "
            & "'white_balance', 'metering_mode', 'iso')"
            & "values ("
            & Q (Pid) & ',' & Q (Exif.Create_Date) & ',' & Q (Exif.Make) & ','
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

   ---------------
   -- Get_Forum --
   ---------------

   function Get_Forum
     (Fid, Tid : in String) return Templates.Translate_Set
   is
      function Get_Fid
        (DBH      : in TLS_DBH_Access;
         Fid, Tid : in String) return String;
      pragma Inline (Get_Fid);
      --  Returns Fid is not empty otherwise compute it using Tid

      -------------
      -- Get_Fid --
      -------------

      function Get_Fid
        (DBH      : in TLS_DBH_Access;
         Fid, Tid : in String) return String
      is
         Line : DB.String_Vectors.Vector;
      begin
         if Fid = "" then
            --  Get the Fid using Tid
            Check_Fid : declare
               Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
            begin
               DBH.Handle.Prepare_Select
                 (Iter,
                  "select forum_id from category, post "
                  & "where category.id = post.category_id "
                  & "and post.id = " & Q (Tid));
               if Iter.More then
                  Iter.Get_Line (Line);

                  Fid : declare
                     Fid : constant String :=
                             DB.String_Vectors.Element (Line, 1);
                  begin
                     Line.Clear;
                     Iter.End_Select;
                     return Fid;
                  end Fid;

               else
                  Logs.Write
                    (Module,
                     Logs.Error,
                     "Get_Id, Fid and Tid empty, raise Database_Error");
                  raise Database_Error;
               end if;
            end Check_Fid;

         else
            return Fid;
         end if;
      end Get_Fid;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;
   begin
      Connect (DBH);

      Get_Forum_Data : declare
         L_Fid : constant String := Get_Fid (DBH, Fid, Tid);
         --  Local Fid computed using Fid or Tid
      begin
         DBH.Handle.Prepare_Select
           (Iter,
            "select name, anonymity, for_photo from forum where id="
            & Q (L_Fid));

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
         end if;
      end Get_Forum_Data;

      return Set;
   end Get_Forum;

   --------------------
   -- Get_Forum_Type --
   --------------------

   function Get_Forum_Type (Tid : in String) return V2P.Database.Forum_Type is
      DBH        : constant TLS_DBH_Access :=
                     TLS_DBH_Access (DBH_TLS.Reference);
      Iter       : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line       : DB.String_Vectors.Vector;
      Forum_Type : V2P.Database.Forum_Type := Forum_Text;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "select for_photo from category, post, forum "
         & "where category.id = post.category_id "
         & "and forum.id = category.forum_id "
         & "and post.id = " & Q (Tid));

      if not Iter.More then
         Logs.Write
           (Module,
            Logs.Error,
            "Get_Id, Fid and Tid empty, raise Database_Error");
         raise Database_Error;
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
     (Filter : in Forum_Filter) return Templates.Translate_Set
   is
      use type Templates.Tag;

      SQL       : constant String := "select id, name, for_photo from forum";
      DBH       : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);

      Set       : Templates.Translate_Set;
      Iter      : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line      : DB.String_Vectors.Vector;
      Id        : Templates.Tag;
      Name      : Templates.Tag;
      For_Photo : Templates.Tag;
      Nb_Lines  : Natural := 0;

   begin
      Connect (DBH);

      if Filter /= Forum_All then
         DBH.Handle.Prepare_Select
           (Iter, SQL & " where for_photo = '"
            & Boolean'Image (Filter = Forum_Photo) & "'");
      else
         DBH.Handle.Prepare_Select (Iter, SQL);
      end if;

      while Iter.More loop
         Nb_Lines := Nb_Lines + 1;
         Iter.Get_Line (Line);

         Id        := Id & DB.String_Vectors.Element (Line, 1);
         Name      := Name & DB.String_Vectors.Element (Line, 2);
         For_Photo := For_Photo & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Forum_List.FID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.FORUM_NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Page_Forum_Threads.FORUM_FOR_PHOTO, Name));

      if Filter /= Forum_All and then Nb_Lines = 1 then

         --  Only one forum matched. Returns the categories too

         Templates.Insert (Set, Get_Categories (Templates.Item (Id, 1)));

      end if;

      return Set;
   end Get_Forums;

   -----------------------
   -- Get_Global_Rating --
   -----------------------

   function Get_Global_Rating
     (Tid : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

      Post_Rating : Templates.Tag;
      Criteria_Id : Templates.Tag;
      Criteria    : Templates.Tag;

   begin
      Connect (DBH);

      --  Get entry information

      DBH.Handle.Prepare_Select
        (Iter, "select post_rating, criteria_id, "
           & "(select name from criteria where id=criteria_id) "
           & "from global_rating where post_id=" & Q (Tid));

      while Iter.More loop
         Iter.Get_Line (Line);

         Post_Rating := Post_Rating & DB.String_Vectors.Element (Line, 1);
         Criteria_Id := Criteria_Id & DB.String_Vectors.Element (Line, 2);
         Criteria    := Criteria    & DB.String_Vectors.Element (Line, 3);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_Global_Rating.GLOBAL_CRITERIA_NAME, Criteria));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_Global_Rating.GLOBAL_CRITERIA_ID, Criteria_Id));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_Global_Rating.GLOBAL_CRITERIA_CURRENT_RATING, Post_Rating));

      return Set;
   end Get_Global_Rating;

   ----------------------
   -- Get_Latest_Posts --
   ----------------------

   function Get_Latest_Posts
     (Limit : in Positive) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String := "select post.id, post.name, filename "
                                   & "from post, forum, photo, category "
                                   & "where post.photo_id = photo.id "
                                   & "and post.category_id = category.id "
                                   & "and category.forum_id = forum.id "
                                   & "and forum.for_photo = 'TRUE' "
                                   & "order by post.date_post DESC "
                                   & "limit" & Positive'Image (Limit);
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Id    : Templates.Tag;
      Name  : Templates.Tag;
      Thumb : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);

      --  Get entry information

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Id    := Id    & DB.String_Vectors.Element (Line, 1);
         Name  := Name  & DB.String_Vectors.Element (Line, 2);
         Thumb := Thumb & DB.String_Vectors.Element (Line, 3);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Latest_Posts.TID, Id));
      Templates.Insert (Set, Templates.Assoc (Block_Latest_Posts.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Block_Latest_Posts.THUMB_SOURCE, Thumb));

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
      SQL   : constant String := "select login from user "
                                   & " order by created DESC "
                                   & " limit " & Positive'Image (Limit);
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

   function Get_Metadata (Pid : in String) return Templates.Translate_Set is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;

   begin
      if Pid = "" then
         --  ???
         return Set;
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select geo_latitude, geo_longitude, "
         & "geo_latitude_formatted, geo_longitude_formatted "
         & "from photo_metadata "
         & "where photo_id = (select photo_id from post where id=" & Pid
         & ')');

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
     (Uid : in String) return Templates.Translate_Set
   is
      SQL : constant String := "select julianday (p.date_post, '+"
        & Utils.Image (Settings.Anonymity_Hours)
        & " hour') - julianday ('now'), datetime (p.date_post, '+"
        & Utils.Image (Settings.Anonymity_Hours)
        & " hour') "
        & "from user_post up, post p "
        & "where up.post_id = p.id and p.photo_id != 0 "
        & "and datetime(p.date_post, '+"
        & Utils.Image (Settings.Anonymity_Hours)
        & " hour') > datetime('now') and up.user_login=" & Q (Uid);

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
     (Tid        : in String;
      Forum_Type : in V2P.Database.Forum_Type)
      return Templates.Translate_Set
   is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      --  Get entry information

      if Forum_Type = Forum_Photo then
         DBH.Handle.Prepare_Select
           (Iter, "select post.name, post.comment, post.hidden, "
            & "filename, width, height, user.login, post.date_post, "
            & "datetime(post.date_post, '+"
            & Utils.Image (Settings.Anonymity_Hours)
            & " hour') < datetime('now') "
            & "from post, user, user_post, photo "
            & "where post.id=" & Q (Tid)
            & " and user.login = user_post.user_login"
            & " and user_post.post_id = post.id"
            & " and photo.id = post.photo_id");

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
                 (Page_Forum_Entry.OWNER,
                  DB.String_Vectors.Element (Line, 7)));

            Templates.Insert
              (Set, Templates.Assoc
                 (Page_Forum_Entry.DATE_POST,
                  DB.String_Vectors.Element (Line, 8)));

            Templates.Insert
              (Set,
               Templates.Assoc
                 (Page_Forum_Entry.REVEALED,
                  DB.String_Vectors.Element (Line, 9)));
            Line.Clear;
         end if;

      else
         DBH.Handle.Prepare_Select
           (Iter, "select post.name, post.comment, post.hidden, "
            & "user.login, post.date_post, "
            & "datetime(post.date_post, '+"
            & Utils.Image (Settings.Anonymity_Hours)
            & " hour') < datetime('now') "
            & "from post, user, user_post "
            & "where post.id=" & Q (Tid)
            & " and user.login = user_post.user_login"
            & " and user_post.post_id = post.id");

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
            Line.Clear;
         end if;
      end if;

      Iter.End_Select;

      return Set;
   end Get_Post;

   -----------------
   -- Get_Threads --
   -----------------

   procedure Get_Threads
     (Fid        : in     String := "";
      User       : in     String := "";
      Admin      : in     Boolean;
      From       : in     Positive := 1;
      Filter     : in     Filter_Mode := All_Messages;
      Order_Dir  : in     Order_Direction := DESC;
      Navigation :    out Post_Ids.Vector;
      Set        :    out Templates.Translate_Set)
   is
      use type Templates.Tag;
      use Post_Ids;

      function Threads_Ordered_Select
        (Fid        : in String := "";
         User       : in String := "";
         Admin      : in Boolean;
         From       : in Positive := 1;
         Filter     : in Filter_Mode := All_Messages;
         Where_Cond : in String := "";
         Order_Dir  : in Order_Direction := DESC;
         Limit      : in Natural := 0) return Unbounded_String;
      --  Returns the select SQL query for listing threads with Filter

      ----------------------------
      -- Threads_Ordered_Select --
      ----------------------------

      function Threads_Ordered_Select
        (Fid        : in String := "";
         User       : in String := "";
         Admin      : in Boolean;
         From       : in Positive := 1;
         Filter     : in Filter_Mode := All_Messages;
         Where_Cond : in String := "";
         Order_Dir  : in Order_Direction := DESC;
         Limit      : in Natural := 0) return Unbounded_String
      is
         SQL_Select  : constant String :=
                         "select post.id, post.name, post.date_post, "
                           & "datetime(date_post, '+"
                           & Utils.Image (Settings.Anonymity_Hours)
                           & " hour') < datetime('now'), "
                           & "(select filename from photo "
                           & "where Id = Post.Photo_Id), "
                           & "category.name, comment_counter,"
                           & "visit_counter, post.hidden, user.login ";
         SQL_From    : constant String :=
                         " from post, category, user, user_post";
         SQL_Where   : constant String :=
                         " where post.category_id = category.id "
                           & " and user_post.post_id = post.id"
                           & " and user_post.user_login = user.login "
                           & Where_Cond;
         Ordering    : constant String :=
                         " order by post.date_post "
                           & Order_Direction'Image (Order_Dir);

         Select_Stmt : Unbounded_String := Null_Unbounded_String;
      begin
         if Fid /= "" then

            if User = "" then
               --  Anonymous login

               Select_Stmt := Select_Stmt & SQL_Select & SQL_From
                 & SQL_Where
                 & " and category.forum_id = " & Q (Fid)
                 & " and user.login = user_post.user_login ";
            else
               --  ???
               Select_Stmt := Select_Stmt & SQL_Select & SQL_From
                 & SQL_Where
                 & "and category.forum_id = " & Q (Fid)
                 & "and user_post.user_id = " & Q (User);
            end if;

            Templates.Insert (Set, Templates.Assoc (Set_Global.FID, Fid));

         elsif User /= "" then
            --  ???

            Select_Stmt := Select_Stmt & SQL_Select & SQL_From
              & SQL_Where
              & " and user_post.user_login = " & Q (User);
         end if;

         if not Admin then
            Select_Stmt := Select_Stmt & " and post.hidden='FALSE'";
         end if;

         --  Add filtering into the select statement

         case Filter is
            when Today =>
               Select_Stmt := Select_Stmt
                 & " and date(post.date_post) = date(current_date)"
                 & Ordering;

            when Two_Days =>
               Select_Stmt := Select_Stmt
                 & " and date(post.date_post) > date(current_date, '-2 days')"
                 & Ordering;

            when Seven_Days =>
               Select_Stmt := Select_Stmt
                 & " and date(post.date_post) > date(current_date, '-7 days')"
                 & Ordering;

            when Fifty_Messages =>
               Select_Stmt := Select_Stmt & Ordering;

               if Limit = 0 then
                  --  SQL offset start to 0 !

                  Select_Stmt := Select_Stmt
                    & " limit 50 offset" & Natural'Image (From - 1);
               end if;

            when All_Messages =>
               Select_Stmt := Select_Stmt & Ordering;
         end case;

         if Limit /= 0 then
            Select_Stmt := Select_Stmt & " limit " & Natural'Image (Limit);
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
      Select_Stmt     : Unbounded_String;

   begin
      Navigation := Post_Ids.Empty_Vector;

      Connect (DBH);

      Select_Stmt := Threads_Ordered_Select
        (Fid       => Fid,
         User      => User,
         Admin     => Admin,
         From      => From,
         Filter    => Filter,
         Order_Dir => Order_Dir);

      if Filter = Fifty_Messages then
         --  ???

         --  Add next and previous information into the translate set

         if From /= 1 then
            Templates.Insert
              (Set,
               Templates.Assoc (Block_Forum_Navigate.PREVIOUS, From - 50));
         end if;

         --  ??? need to check if there is more data !
         Templates.Insert
           (Set,
            Templates.Assoc (Block_Forum_Navigate.NEXT, From + 50));
      end if;

      DBH.Handle.Prepare_Select (Iter, To_String (Select_Stmt));

      while Iter.More loop
         Iter.Get_Line (Line);

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
         Owner           := Owner     & DB.String_Vectors.Element (Line, 10);
         --  Insert this post id in navigation links

         Navigation := Navigation & DB.String_Vectors.Element (Line, 1);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.THUMB_SOURCE, Thumb));

      Templates.Insert (Set, Templates.Assoc (Block_Forum_Threads.TID, Id));
      Templates.Insert (Set, Templates.Assoc (Block_Forum_Threads.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads_Text.DATE_POST, Date_Post));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.CATEGORY, Category));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Forum_Threads.COMMENT_COUNTER, Comment_Counter));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Forum_Threads.VISIT_COUNTER, Visit_Counter));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.REVEALED, Revealed));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.OWNER, Owner));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.HIDDEN, Hidden));
   end Get_Threads;

   -------------------
   -- Get_Thumbnail --
   -------------------

   function Get_Thumbnail (Post : in String) return String is
      DBH      : constant TLS_DBH_Access :=
                   TLS_DBH_Access (DBH_TLS.Reference);
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      Filename : Unbounded_String;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select filename from photo, post "
         & "where photo.id = post.photo_id and post.id = " & Post);

      if Iter.More then
         Iter.Get_Line (Line);

         Filename := To_Unbounded_String (DB.String_Vectors.Element (Line, 1));

         Line.Clear;
      end if;

      Iter.End_Select;

      return To_String (Filename);
   end Get_Thumbnail;

   --------------
   -- Get_User --
   --------------

   function Get_User (Uid : in String) return Templates.Translate_Set is
      use type Templates.Tag;

      U_Data : constant User_Data := Get_User_Data (Uid);
      Set    : Templates.Translate_Set;
   begin
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.HTTP.PASSWORD, U_Data.Password));
      Templates.Insert
        (Set, Templates.Assoc (Set_Global.ADMIN, U_Data.Admin));
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.LOGIN, Uid));
      return Set;
   end Get_User;

   ----------------------
   -- Get_User_Comment --
   ----------------------

   function Get_User_Comment
     (Uid : in String; Textify : in Boolean := False)
      return Templates.Translate_Set
   is
      SQL        : constant String :=
                     "select p.post_id, c.id, comment "
                       & "from comment as c, post_comment as p "
                       & "where user_login = " & Q (Uid)
                       & " and p.comment_id = c.id";
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
         Post_Id    := Post_Id & DB.String_Vectors.Element (Line, 1);
         Comment_Id := Comment_Id & DB.String_Vectors.Element (Line, 2);
         if Textify then
            Comment := Comment
              & Morzhol.Strings.HTML_To_Text
              (DB.String_Vectors.Element (Line, 3));
         else
            Comment := Comment & DB.String_Vectors.Element (Line, 3);
         end if;
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
        (Iter, "select password, admin from user where login=" & Q (Uid));

      if Iter.More then
         Iter.Get_Line (Line);

         Password_Value : declare
            Password : constant String := DB.String_Vectors.Element (Line, 1);
            Admin    : constant String := DB.String_Vectors.Element (Line, 2);
         begin
            Line.Clear;
            return User_Data'(Uid      => +Uid,
                              Password => +Password,
                              Admin    => Boolean'Value (Admin));
         end Password_Value;

      else
         return No_User_Data;
      end if;
   end Get_User_Data;

   -------------------------
   -- Get_User_Last_Photo --
   -------------------------

   function Get_User_Last_Photo
     (Uid : in String) return Templates.Translate_Set is
      SQL : constant String :=
              "select q.photo_id, p.filename "
                & "from user_photo_queue q, photo p "
                & "where q.photo_id = p.id and user_login = " & Q (Uid);
      DBH          : constant TLS_DBH_Access :=
                       TLS_DBH_Access (DBH_TLS.Reference);

      Set          : Templates.Translate_Set;
      Iter         : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line         : DB.String_Vectors.Vector;
   begin
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
                       "select content, content_html from user_page "
                         & "where user_login=" & Q (Uid);
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

      return Set;
   end Get_User_Page;

   -----------------------------
   -- Get_User_Rating_On_Post --
   -----------------------------

   function Get_User_Rating_On_Post
     (Uid : in String; Tid : in String) return Templates.Translate_Set
   is
      use AWS.Templates;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

      Post_Rating : Templates.Tag;
      Criteria_Id : Templates.Tag;
      Criteria    : Templates.Tag;

   begin
      Connect (DBH);

      --  Get entry information

      DBH.Handle.Prepare_Select
        (Iter, "select id, name, (select post_rating from rating r "
         & "where r.post_id=" & Q (Tid)
         & " and r.user_login=" & Q (Uid)
         & " and criteria_id=id) from criteria");

      while Iter.More loop
         Iter.Get_Line (Line);

         Criteria_Id := Criteria_Id & DB.String_Vectors.Element (Line, 1);
         Criteria    := Criteria    & DB.String_Vectors.Element (Line, 2);
         Post_Rating := Post_Rating & DB.String_Vectors.Element (Line, 3);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_New_Comment.CRITERIA_NAME, Criteria));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_New_Comment.CRITERIA_ID, Criteria_Id));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_New_Comment.CRITERIA_CURRENT_RATING, Post_Rating));

      return Set;
   end Get_User_Rating_On_Post;

   -------
   -- I --
   -------

   function I (Int : in Integer) return String is
   begin
      return Integer'Image (Int);
   end I;

   -----------------------------
   -- Increment_Visit_Counter --
   -----------------------------

   procedure Increment_Visit_Counter (Pid : in String) is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "update post set visit_counter = visit_counter + 1 where "
                & "id = " & Q (Pid);
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
      Thread    : in String;
      Name      : in String;
      Comment   : in String;
      Pid       : in String) return String
   is
      pragma Unreferenced (Name);

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String);
      --  Insert row into Comment table

      procedure Insert_Table_Post_Comment (post_Id, Comment_Id : in String);
      --  Insert row into post_Comment table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      --------------------------
      -- Insert_Table_Comment --
      --------------------------

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String)
      is
         SQL : constant String :=
                 "insert into comment ('user_login', 'anonymous_user', "
                   & "'comment', 'photo_id')"
                   & " values ("
                   & Q (User_Login) & ',' & Q (Anonymous) & ',' & Q (Comment)
                   & ',' & Q (Pid) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Comment;

      --------------------------------
      -- Insert_Table_post_Comment --
      --------------------------------

      procedure Insert_Table_Post_Comment
        (post_Id, Comment_Id : in String)
      is
         SQL : constant String :=
                 "insert into post_comment values ("
                   & post_Id & "," & Comment_Id & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Post_Comment;

   begin
      Connect (DBH);
      DBH.Handle.Begin_Transaction;
      Insert_Table_Comment (Uid, Anonymous, Comment);

      Row_Id : declare
         Cid : constant String := DBH.Handle.Last_Insert_Rowid;
      begin
         Insert_Table_Post_Comment (Thread, Cid);
         DBH.Handle.Commit;
         return Cid;
      end Row_Id;
   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
         return "";
   end Insert_Comment;

   ---------------------
   -- Insert_Metadata --
   ---------------------

   procedure Insert_Metadata
     (Pid                     : in String;
      Geo_Latitude            : in Float;
      Geo_Longitude           : in Float;
      Geo_Latitude_Formatted  : in String;
      Geo_Longitude_Formatted : in String)
   is
      SQL : constant String := "insert into photo_metadata (photo_id, "
        & "geo_latitude, geo_longitude, geo_latitude_formatted, "
        & "geo_longitude_formatted) values ("
        & "(select photo_id from post where id=" & Pid & "), "
        & F (Geo_Latitude) & ", " & F (Geo_Longitude) & ", "
        & Q (Geo_Latitude_Formatted) & ", "
        & Q (Geo_Longitude_Formatted) & ")";

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Text_IO.Put_Line (Exception_Message (E));
   end Insert_Metadata;

   ------------------
   -- Insert_Photo --
   ------------------

   function Insert_Photo
     (Uid         : in String;
      Filename    : in String;
      Height      : in Integer;
      Width       : in Integer;
      Size        : in Integer) return String
   is

      procedure Insert_Table_Photo
        (Filename : in String;
         Height   : in Integer;
         Width    : in Integer;
         Size     : in Integer);
      --  Insert row into the photo table

      procedure User_Tmp_Photo (Uid, Pid : in String);
      --  Update user_photo_queue table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      ------------------------
      -- Insert_Table_Photo --
      ------------------------

      procedure Insert_Table_Photo
        (Filename : in String;
         Height   : in Integer;
         Width    : in Integer;
         Size     : in Integer) is
         SQL : constant String :=
           "insert into photo ('filename', 'height', 'width', 'size') "
           & "values (" & Q (Filename) & ',' & I (Height) & ','
           & I (Width) & ',' & I (Size) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Photo;

      --------------------
      -- User_Tmp_Photo --
      --------------------

      procedure User_Tmp_Photo (Uid, Pid : in String) is
         SQL : constant String :=
                 "update user_photo_queue set photo_id = " & Pid
                   & " where user_login = " & Q (Uid);
      begin
         DBH.Handle.Execute (SQL);
      end User_Tmp_Photo;

   begin
      Connect (DBH);

      DBH.Handle.Begin_Transaction;

      Insert_Table_Photo (Filename, Height, Width, Size);

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
      Category_Id : in String;
      Name        : in String;
      Comment     : in String;
      Pid         : in String) return String
   is
      procedure Insert_Table_Post
        (Name, Category_Id, Comment, Photo_Id : in String);
      --  Insert row into the post table

      procedure Insert_Table_User_Post (Uid, Post_Id : in String);
      --  Insert row into the user_post table

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      ------------------------
      -- Insert_Table_post --
      ------------------------

      procedure Insert_Table_Post
        (Name, Category_Id, Comment, Photo_Id : in String)
      is
         SQL : constant String :=
                 "insert into post ('name', 'comment', 'category_id',"
                   & "'template_id', 'visit_counter', 'comment_counter',"
                   & "'photo_id') values (" & Q (Name) &  ',' & Q (Comment)
                   & ',' & Category_Id & ", 1, 0, 0," & Photo_Id & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Post;

      -----------------------------
      -- Insert_Table_User_post --
      -----------------------------

      procedure Insert_Table_User_Post (Uid, Post_Id : in String) is
         SQL : constant String :=
                 "insert into user_post values ("
                   & Q (Uid) & ',' & Post_Id & ")";
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_User_Post;

   begin
      Connect (DBH);

      DBH.Handle.Begin_Transaction;

      if Pid /= "" then
         Insert_Table_Post (Name, Category_Id, Comment, Pid);
      else
         Insert_Table_Post (Name, Category_Id, Comment, "NULL");
      end if;

      Row_Id : declare
         Post_Id : constant String := DBH.Handle.Last_Insert_Rowid;
      begin
         Insert_Table_User_Post (Uid, Post_Id);
         DBH.Handle.Commit;
         return Post_Id (Post_Id'First + 1 .. Post_Id'Last);
      end Row_Id;

   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
         return "";
      when E : others =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
         return "";
   end Insert_Post;

   ---------------
   -- Is_Author --
   ---------------

   function Is_Author (Uid, Pid : in String) return Boolean is
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Result : Boolean := False;
   begin
      Connect (DBH);

      --  Get post Pid posted by user Uid

      DBH.Handle.Prepare_Select
        (Iter,
         "select * from user_post where post_id  = "
           & Q (Pid) & " and user_login = " & Q (Uid));

      if Iter.More then
         Result := True;
      end if;

      Iter.End_Select;

      return Result;
   end Is_Author;

   -------
   -- Q --
   -------

   function Q (Str : in String) return String is
      S : String (1 .. 2 + Str'Length * 2);
      J : Positive := S'First;
   begin
      if Str = "" then
         return "NULL";
      end if;

      S (J) := ''';

      for K in Str'Range loop
         if Str (K) = ''' then
            J := J + 1;
            S (J) := ''';
         end if;
         J := J + 1;
         S (J) := Str (K);
      end loop;

      J := J + 1;
      S (J) := ''';

      return S (1 .. J);
   end Q;

   function Q (Str : in Unbounded_String) return String is
   begin
      return Q (To_String (Str));
   end Q;

   function Q (Bool : in Boolean) return String is
   begin
      return Q (Boolean'Image (Bool));
   end Q;

   --------------------------
   -- Toggle_Hidden_Status --
   --------------------------

   function Toggle_Hidden_Status
     (Tid : in String) return Templates.Translate_Set
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
        (Iter, "select hidden from post where post.id=" & Q (Tid));

      if Iter.More then
         Iter.Get_Line (Line);
         Hidden := Boolean'Value (DB.String_Vectors.Element (Line, 1));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Toggle and store new status

      Hidden := not Hidden;

      DBH.Handle.Execute
        ("update post set hidden=" & Q (Hidden) & " where id=" & Q (Tid));

      Templates.Insert
        (Set, Templates.Assoc
           (Page_Forum_Entry.HIDDEN, Boolean'Image (Hidden)));
      return Set;

   exception
      when E : DB.DB_Error =>
         Text_IO.Put_Line (Exception_Message (E));
         return Set;
   end Toggle_Hidden_Status;

   -----------------
   -- Update_Page --
   -----------------

   procedure Update_Page
     (Uid : in String; Content : in String; Content_HTML : in String)
   is
      SQL : constant String :=
              "update user_page set content_html =  " & Q (Content_HTML)
              & ", content=" & Q (Content) & " where user_login=" & Q (Uid);

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Update_Page;

   -------------------
   -- Update_Rating --
   -------------------

   procedure Update_Rating
     (Uid      : in String;
      Tid      : in String;
      Criteria : in String;
      Value    : in String)
   is
      SQL  : constant String :=
               "select 1 from rating where user_login="
                 & Q (Uid) & " and post_id=" & Q (Tid) & " and criteria_id="
                 & Q (Criteria);
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         --  Need update
         Iter.End_Select;
         DBH.Handle.Execute ("update rating set post_rating = " & Q (Value)
                             & "where user_login="
                             & Q (Uid) & " and post_id=" & Q (Tid)
                             & " and criteria_id=" & Q (Criteria));
      else
         --  Insert new rating
         Iter.End_Select;
         DBH.Handle.Execute ("insert into rating values (" & Q (Uid)
                             & ", " & Q (Tid) & ", " & Q (Criteria)
                             & ", " & Q (Value) & ")");
      end if;
   end Update_Rating;

end V2P.Database;
