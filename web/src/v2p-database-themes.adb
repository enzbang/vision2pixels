------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2012                            --
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

with AWS.Utils;

with V2P.Database.Support;
with V2P.Database.Timezone;
with V2P.DB_Handle;
with V2P.Settings;

with V2P.Template_Defs.Page_Theme;
with V2P.Template_Defs.Block_Theme_Admin;
with V2P.Template_Defs.Block_Theme_List;
with V2P.Template_Defs.Block_Theme_Photos;
with V2P.Template_Defs.Block_Theme_Status;

package body V2P.Database.Themes is

   use V2P.Database.Support;
   use V2P.DB_Handle;

   Lock : Utils.Semaphore;

   Stages : constant array (0 .. 3) of Stage :=
              (Open, Stage_1, Stage_2, Closed);
   --  Value to Stage representation

   C_Stage           : Stage := Closed;
   C_Theme           : Integer := 0;
   Cache_Initialized : Boolean := False;

   procedure Get_Selected_Photos
     (Login    : in String;
      Selected : out Templates.Tag;
      Count    : out Natural);
   --  Returns the current selected photos (and the corresponding count) for
   --  the given user.

   procedure Set_Cached_Data;
   --  Set the cached data for fast access

   ------------
   -- Create --
   ------------

   function Create (Title : in String) return Boolean is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
              "INSERT INTO themes ('title', 'stage')"
              & " VALUES (" & Q (Title) & ", 0)";
   begin
      Connect (DBH);

      Lock.Seize;

      if Current_Stage = Closed then
         DBH.Handle.Execute (SQL);
         C_Stage := Open;
      else
         return False;
      end if;

      Lock.Release;
      return True;
   exception
      when others =>
         Lock.Release;
         Cache_Initialized := False;
         return False;
   end Create;

   -------------------
   -- Current_Stage --
   -------------------

   function Current_Stage return Stage is
   begin
      if not Cache_Initialized then
         Set_Cached_Data;
         Cache_Initialized := True;
      end if;

      return C_Stage;
   end Current_Stage;

   ----------------------
   -- Get_Admin_Status --
   ----------------------

   function Get_Admin_Status return Templates.Translate_Set is
      use type Templates.Tag;

      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT COUNT(user_login) as cnt, photo_id"
                & " FROM themes_user_votes"
                & " WHERE stage=" & I (Stage'Pos (Current_Stage))
                & " GROUP BY photo_id"
                & " ORDER BY cnt DESC, photo_id ASC LIMIT 5";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Count : Templates.Tag;
      Photo : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Count := Count & DB.String_Vectors.Element (Line, 1);
         Photo := Photo & DB.String_Vectors.Element (Line, 2);
         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Theme_Admin.PHOTO_ID, Photo));
      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Theme_Admin.NB_VOTE, Count));

      return Set;
   end Get_Admin_Status;

   ------------------------
   -- Get_Current_Photos --
   ------------------------

   function Get_Current_Photos
     (Login : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;

      function Max_Vote return Positive;
      --  Returns the maximum number of vote for the given stage

      --------------
      -- Max_Vote --
      --------------

      function Max_Vote return Positive is
      begin
         if Current_Stage = Stage_1 then
            return Settings.Max_Theme_Vote_Per_User;
         else
            return 1;
         end if;
      end Max_Vote;

      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT post.id, post.name, photo.filename, photo.id, "
                & "themes_photos.stage, user_post.user_login"
                & " FROM post, photo, themes, themes_photos, user_post "
                & " WHERE themes.stage!=" & I (Stage'Pos (Closed))
                & "  AND themes.id=themes_photos.theme_id"
                & "  AND post.photo_id=themes_photos.photo_id"
                & "  AND photo.id=post.photo_id"
                & "  AND user_post.post_id=post.id";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Id    : Templates.Tag;
      P_Id  : Templates.Tag;
      Name  : Templates.Tag;
      Thumb : Templates.Tag;
      Sel   : Templates.Tag;
      Vote  : Templates.Tag;
      Count : Natural := 0;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Id    := Id    & DB.String_Vectors.Element (Line, 1);
         Name  := Name  & DB.String_Vectors.Element (Line, 2);
         Thumb := Thumb & DB.String_Vectors.Element (Line, 3);
         P_Id  := P_Id  & DB.String_Vectors.Element (Line, 4);

         --  We can votet only for photos that have been selected for the
         --  current stage and if it is not our own photo.

         if Current_Stage = Open
           or else
             Integer'Value
               (DB.String_Vectors.Element
                  (Line, 5)) /= Stage'Pos (Current_Stage) - 1
           or else
             DB.String_Vectors.Element (Line, 6) = Login
         then
            Vote := Vote & False;
         else
            Vote := Vote & True;
         end if;

         Line.Clear;
      end loop;

      Iter.End_Select;

      --  Get selected photos if any

      if Login /= "" then
         Get_Selected_Photos (Login, Sel, Count);

         Templates.Insert
           (Set, Templates.Assoc
              (Template_Defs.Block_Theme_Photos.SELECTED_PHOTOS, Sel));
      end if;

      --  We can only vote if ...

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_Photos.CAN_VOTE,
            --  We are authenticated
            Login /= ""
            --  The theme is in stage 1 or 2
            and then Current_Stage in Stage_1 .. Stage_2
            --  The user has still some vote
            and then Count < Max_Vote));

      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Theme_Photos.TID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Theme_Photos.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_Photos.THUMB_SOURCE, Thumb));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_Photos.PHOTO_ID, P_Id));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_Photos.VOTE, Vote));

      return Set;
   end Get_Current_Photos;

   ------------------------
   -- Get_Current_Status --
   ------------------------

   function Get_Current_Status
     (TZ : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL  : constant String :=
               "SELECT title, "
               & Timezone.Date_Time ("created", TZ) & ", stage, "
               & "(SELECT count(*) FROM themes_photos"
               & " WHERE themes.id=theme_id)"
               & " FROM themes WHERE stage != " & I (Stage'Pos (Closed));
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Block_Theme_Status.THEME_TITLE,
               DB.String_Vectors.Element (Line, 1)));
         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Block_Theme_Status.THEME_CREATE_DATE,
               DB.String_Vectors.Element (Line, 2)));

         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Block_Theme_Status.THEME_STAGE,
               Themes.Stage'Image
                 (Stages
                    (Natural'Value (DB.String_Vectors.Element (Line, 3))))));

         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Block_Theme_Status.THEME_NB_PHOTOS,
               DB.String_Vectors.Element (Line, 4)));
      end if;

      Iter.End_Select;

      return Set;
   end Get_Current_Status;

   -------------------------
   -- Get_Selected_Photos --
   -------------------------

   procedure Get_Selected_Photos
     (Login    : in String;
      Selected : out Templates.Tag;
      Count    : out Natural)
   is
      use type Templates.Tag;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String :=
                "SELECT themes_photos.photo_id"
                & " FROM themes, themes_photos, themes_user_votes"
                & " WHERE user_login=" & Q (Login)
                & " AND themes_user_votes.photo_id "
                & "   = themes_photos.photo_id"
                & " AND themes_photos.theme_id = themes.id"
                & " AND themes.stage != " & I (Stage'Pos (Closed))
                & " AND themes_user_votes.stage = "
                & I (Stage'Pos (Current_Stage));
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Count := 0;

      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);
         Selected := Selected & DB.String_Vectors.Element (Line, 1);
         Count := Count + 1;

         Line.Clear;
      end loop;

      Iter.End_Select;
   end Get_Selected_Photos;

   --------------------
   -- Get_Theme_Data --
   --------------------

   function Get_Theme_Data
     (TID : in Id; TZ : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;

      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      D_SQL : constant String :=
                "SELECT title, "
                & Timezone.Date_Time ("created", TZ)
                & " FROM themes WHERE id=" & I (TID);
      P_SQL : constant String :=
                "SELECT post.id, post.name, photo.filename, photo.id, "
                & "themes_photos.stage"
                & " FROM post, photo, themes, themes_photos "
                & " WHERE themes.id=" & I (TID)
                & "  AND themes.id=themes_photos.theme_id"
                & "  AND post.photo_id=themes_photos.photo_id"
                & "  AND photo.id=post.photo_id"
                & " ORDER BY themes_photos.stage DESC";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Id    : Templates.Tag;
      Name  : Templates.Tag;
      Thumb : Templates.Tag;
      Stage : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, D_SQL);

      --  Data

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Page_Theme.THEME_TITLE,
               DB.String_Vectors.Element (Line, 1)));
         Templates.Insert
           (Set,
            Templates.Assoc
              (Template_Defs.Page_Theme.THEME_DATE,
               DB.String_Vectors.Element (Line, 2)));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Photos

      DBH.Handle.Prepare_Select (Iter, P_SQL);

      --  Data

      while Iter.More loop
         Iter.Get_Line (Line);

         Id    := Id    & DB.String_Vectors.Element (Line, 1);
         Name  := Name  & DB.String_Vectors.Element (Line, 2);
         Thumb := Thumb & DB.String_Vectors.Element (Line, 3);
         Stage := Stage & DB.String_Vectors.Element (Line, 5);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Theme.TID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Theme.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Theme.THUMB_SOURCE, Thumb));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Theme.PHOTO_STAGE, Stage));

      return Set;
   end Get_Theme_Data;

   ----------------
   -- Get_Themes --
   ----------------

   function Get_Themes return Templates.Translate_Set is
      use type Templates.Tag;

      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT themes.id, title, post.id, post.name, photo.filename"
                & " FROM themes, post, photo, themes_photos"
                & " WHERE themes.stage=" & I (Stage'Pos (Closed))
                & "   AND themes.id = themes_photos.theme_id"
                & "   AND themes_photos.photo_id = photo.id"
                & "   AND photo.id = post.photo_id"
                & "   AND themes_photos.stage = 2"
                & " ORDER BY themes.created DESC";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Title : Templates.Tag;
      T_Id  : Templates.Tag;
      Id    : Templates.Tag;
      Name  : Templates.Tag;
      Thumb : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         T_Id  := T_Id & DB.String_Vectors.Element (Line, 1);
         Title := Title & DB.String_Vectors.Element (Line, 2);
         Id    := Id & DB.String_Vectors.Element (Line, 3);
         Name  := Name & DB.String_Vectors.Element (Line, 4);
         Thumb := Thumb & DB.String_Vectors.Element (Line, 5);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_List.THEME_ID, T_Id));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Theme_List.THEME_TITLE, Title));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Theme.TID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Theme.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Theme.THUMB_SOURCE, Thumb));
      return Set;
   end Get_Themes;

   --------------------
   -- Get_Vote_Count --
   --------------------

   function Get_Vote_Count (Login : in String) return Natural is
      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT COUNT(*) FROM themes, themes_photos, themes_user_votes"
                & " WHERE user_login=" & Q (Login)
                & " AND themes_user_votes.photo_id = themes_photos.photo_id"
                & " AND themes_photos.theme_id = themes.id"
                & " AND themes.stage = themes_user_votes.stage";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Count : Natural := 0;
   begin
      Connect (DBH);

      --  Check if current vote exists or not

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         Count := Natural'Value (DB.String_Vectors.Element (Line, 1));
         Line.Clear;
      end if;

      Iter.End_Select;

      return Count;
   end Get_Vote_Count;

   ----------------
   -- Next_Stage --
   ----------------

   procedure Next_Stage is
   begin
      if Current_Stage in Theme_Running then
         declare
            DBH : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);
         begin
            Connect (DBH);

            DBH.Handle.Begin_Transaction;

            --  First select the photos for the next stage

            if Current_Stage in Stage_1 .. Stage_2 then
               declare
                  SQL  : constant String :=
                           "SELECT COUNT(user_login) as cnt, photo_id"
                           & " FROM themes_user_votes"
                           & " WHERE stage=" & I (Stage'Pos (Current_Stage))
                           & " GROUP BY photo_id"
                           & " ORDER BY cnt DESC, photo_id ASC LIMIT 2";
                  Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
                  Line : DB.String_Vectors.Vector;
               begin
                  DBH.Handle.Prepare_Select (Iter, SQL);

                  while Iter.More loop
                     Iter.Get_Line (Line);

                     DBH.Handle.Execute
                       ("UPDATE themes_photos SET stage=stage+1"
                        & " WHERE photo_id="
                        & Q (DB.String_Vectors.Element (Line, 2))
                        & " AND theme_id=" & I (C_Theme));

                     Line.Clear;
                  end loop;
               end;
            end if;

            --  Then move to next stage

            DBH.Handle.Execute
              ("UPDATE themes SET stage=stage+1"
               & " WHERE stage=" & I (Stage'Pos (Current_Stage)));
            C_Stage := Stage'Succ (C_Stage);

            DBH.Handle.Commit;
         exception
            when others =>
               --  Invalidate the cache, not sure the request was performed
               Cache_Initialized := False;
               DBH.Handle.Rollback;
         end;
      end if;
   end Next_Stage;

   ---------------------
   -- Set_Cached_Data --
   ---------------------

   procedure Set_Cached_Data is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL  : constant String :=
               "SELECT id, stage FROM themes ORDER BY id DESC LIMIT 1";
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         C_Theme := Natural'Value (DB.String_Vectors.Element (Line, 1));
         C_Stage := Stages
           (Natural'Value (DB.String_Vectors.Element (Line, 2)));
      end if;

      Iter.End_Select;
   end Set_Cached_Data;

   --------------------
   -- Set_Reset_Vote --
   --------------------

   procedure Set_Reset_Vote (Login, Photo_Id : in String) is
      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT * FROM themes_user_votes WHERE user_login="
                & Q (Login) & " AND photo_id=" & Q (Photo_Id)
                & " AND stage=" & I (Stage'Pos (Current_Stage));
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Exist : Boolean;
   begin
      Connect (DBH);

      --  ??? should we check if the max count has been reached? (fast click)

      --  Check if current vote exists or not

      DBH.Handle.Prepare_Select (Iter, SQL);
      Exist := Iter.More;
      Iter.End_Select;

      if Exist then
         --  This vote exists, remove it
         DBH.Handle.Execute
           ("DELETE FROM themes_user_votes "
            & " WHERE user_login=" & Q (Login)
            & " AND stage=" & I (Stage'Pos (Current_Stage))
            & " AND photo_id=" & Q (Photo_Id));
      else
         --  Add this vote for the user
         DBH.Handle.Execute
           ("INSERT INTO themes_user_votes ('user_login', 'photo_id', 'stage')"
            & " VALUES (" & Q (Login) & ", "
            & Q (Photo_Id) & ','
            & I (Stage'Pos (Current_Stage)) & ')');
      end if;
   end Set_Reset_Vote;

end V2P.Database.Themes;
