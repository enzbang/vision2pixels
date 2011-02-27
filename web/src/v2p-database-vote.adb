------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2010-2011                          --
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

with V2P.Template_Defs.Block_Cdc;
with V2P.Template_Defs.Block_Cdc_Data;
with V2P.Template_Defs.Block_Cdc_Info;
with V2P.Template_Defs.Block_Global_Rating;
with V2P.Template_Defs.Block_New_Vote;
with V2P.Template_Defs.Block_Photo_Of_The_Week;
with V2P.Template_Defs.Block_User_Voted_Photos_List;
with V2P.Template_Defs.Chunk_List_Navlink;
with V2P.Template_Defs.Set_Global;

package body V2P.Database.Vote is

   use V2P.Database.Support;
   use V2P.Template_Defs;

   -------------
   -- Get_CdC --
   -------------

   function Get_CdC (From : in Positive) return Templates.Translate_Set is
      DBH         : constant TLS_DBH_Access :=
                      TLS_DBH_Access (DBH_TLS.Reference);
      SQL         : constant String :=
                      "SELECT q.post_id, p.filename, q.elected_on, "
                        & "o.comment_counter, o.visit_counter, c.name, o.name "
                        & "FROM photo_of_the_week q, photo p, post o, "
                        & "category c "
                        & "WHERE q.post_id=o.id"
                        & " AND p.id=o.photo_id AND o.category_id=c.id"
                        & " ORDER BY q.elected_on DESC"
                        & " LIMIT " & Utils.Image (Settings.Number_CdC_Listed)
                        & " OFFSET " & Utils.Image (From - 1);

      Set         : Templates.Translate_Set;
      Iter        : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line        : DB.String_Vectors.Vector;
      TIDs        : Templates.Tag;
      Thumbs      : Templates.Tag;
      Date        : Templates.Tag;
      Visits      : Templates.Tag;
      Comments    : Templates.Tag;
      Categories  : Templates.Tag;
      Names       : Templates.Tag;
      Lines       : Natural := 0;
      Total_Lines : Natural := 0;
   begin
      Connect (DBH);

      --  Compute total number of CdC

      DBH.Handle.Prepare_Select
        (Iter, "SELECT count(*) FROM photo_of_the_week WHERE post_id!=0");

      if Iter.More then
         Iter.Get_Line (Line);
         Total_Lines := Positive'Value (DB.String_Vectors.Element (Line, 1));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Returns CdC

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);
         Lines := Lines + 1;

         Templates.Append (TIDs, DB.String_Vectors.Element (Line, 1));
         Templates.Append (Thumbs, DB.String_Vectors.Element (Line, 2));
         Templates.Append (Date, DB.String_Vectors.Element (Line, 3));
         Templates.Append (Comments, DB.String_Vectors.Element (Line, 4));
         Templates.Append (Visits, DB.String_Vectors.Element (Line, 5));
         Templates.Append (Categories, DB.String_Vectors.Element (Line, 6));
         Templates.Append (Names,  DB.String_Vectors.Element (Line, 7));
         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.TID, TIDs));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.THUMB_SOURCE, Thumbs));
      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Cdc.ELECTED_ON, Date));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Cdc.COMMENT_COUNTER, Comments));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.VISIT_COUNTER, Visits));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.CATEGORY, Categories));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.NAME, Names));

      Templates.Insert
        (Set, Templates.Assoc (Set_Global.NAV_FROM, From));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Chunk_List_Navlink.NB_LINE_RETURNED, Lines));
      Templates.Insert
        (Set, Templates.Assoc
           (Chunk_List_Navlink.NAV_NB_LINES_TOTAL, Total_Lines));
      return Set;
   end Get_CdC;

   ------------------
   -- Get_CdC_Data --
   ------------------

   function Get_CdC_Data (Tid : in Id) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH      : constant TLS_DBH_Access :=
                   TLS_DBH_Access (DBH_TLS.Reference);
      Set      : Templates.Translate_Set;
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      Electors : Templates.Tag;
   begin
      Connect (DBH);

      --  Get score and date

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT id, val, " & Timezone.Date ("elected_on", "")
         & " FROM photo_of_the_week WHERE post_id=" & To_String (Tid));

      if Iter.More then
         Iter.Get_Line (Line);

         declare
            Week_Id : constant String :=
                        DB.String_Vectors.Element (Line, 1);
         begin
            Templates.Insert
              (Set,
               Templates.Assoc
                 (Template_Defs.Block_Cdc_Data.SCORE,
                  DB.String_Vectors.Element (Line, 2)));
            Templates.Insert
              (Set,
               Templates.Assoc
                 (Template_Defs.Block_Cdc_Data.DATE,
                  DB.String_Vectors.Element (Line, 3)));

            Line.Clear;

            Iter.End_Select;

            --  Get electors

            DBH.Handle.Prepare_Select
              (Iter,
               "SELECT user_login FROM user_photo_of_the_week"
               & " WHERE post_id=" & To_String (Tid)
               & " AND week_id = " & Q (Week_Id));

            while Iter.More loop
               Iter.Get_Line (Line);

               Electors := Electors & DB.String_Vectors.Element (Line, 1);
               Line.Clear;
            end loop;

            Templates.Insert
              (Set,
               Templates.Assoc (Template_Defs.Block_Cdc_Data.USERS, Electors));
         end;
      end if;

      Iter.End_Select;

      return Set;
   end Get_CdC_Data;

   ------------------
   -- Get_CdC_Info --
   ------------------

   function Get_CdC_Info return Templates.Translate_Set is
      DBH        : constant TLS_DBH_Access :=
                     TLS_DBH_Access (DBH_TLS.Reference);
      SQL        : constant String :=
                     "SELECT COUNT(DISTINCT(user_login)), "
                       & "COUNT(DISTINCT(post_id)) "
                       & "FROM user_photo_of_the_week WHERE week_id=0";
      Set        : Templates.Translate_Set;
      Iter       : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line       : DB.String_Vectors.Vector;
      Nb_Elector : Natural := 0;
      Nb_Photo   : Natural := 0;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         Nb_Elector := Positive'Value (DB.String_Vectors.Element (Line, 1));
         Nb_Photo := Positive'Value (DB.String_Vectors.Element (Line, 2));
         Line.Clear;
      end if;

      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Cdc_Info.NB_ELECTOR, Nb_Elector));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Block_Cdc_Info.NB_PHOTO, Nb_Photo));
      return Set;
   end Get_CdC_Info;

   -----------------------
   -- Get_Global_Rating --
   -----------------------

   function Get_Global_Rating (Tid : in Id) return Templates.Translate_Set is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;

      Post_Rating : Templates.Tag;
      Criteria_Id : Templates.Tag;
      Criteria    : Templates.Tag;

      Nb_Vote     : Natural := 0;

   begin
      Connect (DBH);

      --  Get entry information

      DBH.Handle.Prepare_Select
        (Iter, "SELECT post_rating, criteria_id, "
           & "(SELECT name FROM criteria WHERE id=criteria_id), "
           & "nb_vote FROM global_rating WHERE post_id=" & To_String (Tid));

      while Iter.More loop
         Iter.Get_Line (Line);

         Post_Rating := Post_Rating & DB.String_Vectors.Element (Line, 1);
         Criteria_Id := Criteria_Id & DB.String_Vectors.Element (Line, 2);
         Criteria    := Criteria    & DB.String_Vectors.Element (Line, 3);

         if Nb_Vote < Natural'Value (DB.String_Vectors.Element (Line, 4)) then
            Nb_Vote := Natural'Value (DB.String_Vectors.Element (Line, 4));
         end if;
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

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_Global_Rating.GLOBAL_NB_VOTE, Nb_Vote));
      return Set;
   end Get_Global_Rating;

   ---------------------------
   -- Get_Photo_Of_The_Week --
   ---------------------------

   function Get_Photo_Of_The_Week return Templates.Translate_Set is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "SELECT val, w.post_id, photo.filename,"
         & " photo.width, photo.height"
         & " FROM photo_of_the_week w, post, photo"
         & " WHERE post.id=w.post_id AND post.photo_id=photo.id"
         & " ORDER BY w.id DESC LIMIT 1");

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Photo_Of_The_Week.PHOTO_OF_THE_WEEK_SCORE,
                 DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Photo_Of_The_Week.PHOTO_OF_THE_WEEK_URL,
               DB.String_Vectors.Element (Line, 2)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Photo_Of_The_Week.PHOTO_OF_THE_WEEK_IMG_SOURCE,
               DB.String_Vectors.Element (Line, 3)));

         Templates.Insert
           (Set, Templates.Assoc
              (Block_Photo_Of_The_Week.PHOTO_OF_THE_WEEK_WIDTH,
               DB.String_Vectors.Element (Line, 4)));
         Templates.Insert
           (Set, Templates.Assoc
              (Block_Photo_Of_The_Week.PHOTO_OF_THE_WEEK_HEIGHT,
               DB.String_Vectors.Element (Line, 5)));
      end if;

      return Set;
   end Get_Photo_Of_The_Week;

   -----------------------------
   -- Get_User_Rating_On_Post --
   -----------------------------

   function Get_User_Rating_On_Post
     (Uid : in String; Tid : in Id) return Templates.Translate_Set
   is
      use type AWS.Templates.Tag;

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
        (Iter, "SELECT id, name, (SELECT post_rating FROM rating r "
         & "WHERE r.post_id=" & To_String (Tid)
         & " AND r.user_login=" & Q (Uid)
         & " AND criteria_id=id) FROM criteria");

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
           (Block_New_Vote.CRITERIA_NAME, Criteria));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_New_Vote.CRITERIA_ID, Criteria_Id));

      Templates.Insert
        (Set,
         Templates.Assoc
           (Block_New_Vote.CRITERIA_CURRENT_RATING, Post_Rating));

      return Set;
   end Get_User_Rating_On_Post;

   ---------------------------
   -- Get_User_Voted_Photos --
   ---------------------------

   function Get_User_Voted_Photos
     (Uid : in String) return Templates.Translate_Set
   is
      use type AWS.Templates.Tag;

      DBH         : constant TLS_DBH_Access :=
                      TLS_DBH_Access (DBH_TLS.Reference);
      SQL         : constant String :=
                      "SELECT w.post_id, photo.filename "
                        & "FROM user_photo_of_the_week w, photo, post "
                        & "WHERE w.post_id = post.id "
                        & "AND post.photo_id = photo.id "
                        & "AND week_id=0 "
                        & "AND w.user_login = " & Q (Uid);

      Set         : Templates.Translate_Set;
      Iter        : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line        : DB.String_Vectors.Vector;
      Ids, Thumbs : Templates.Tag;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Ids    := Ids    & DB.String_Vectors.Element (Line, 1);
         Thumbs := Thumbs & DB.String_Vectors.Element (Line, 2);
         Line.Clear;
      end loop;

      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Voted_Photos_List.TID, Ids));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_User_Voted_Photos_List.THUMB_SOURCE, Thumbs));

      Iter.End_Select;
      return Set;
   end Get_User_Voted_Photos;

   -------------------
   -- Has_User_Vote --
   -------------------

   function Has_User_Vote (Uid : in String; Tid : in Id) return Boolean is
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      SQL    : constant String :=
                 "SELECT * FROM user_photo_of_the_week "
                   & "WHERE user_login=" & Q (Uid)
                   & " AND post_id=" & To_String (Tid)
                   & " AND week_id=0";
      --  week_id=0 as we want only the photo for which the user has voted for
      --  the current open vote.
      Result : Boolean := False;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Result := True;
      end if;

      Iter.End_Select;

      return Result;
   end Has_User_Vote;

   ----------------------------
   -- Toggle_Vote_Week_Photo --
   ----------------------------

   procedure Toggle_Vote_Week_Photo (Uid : in String; Tid : in Id) is
      DBH      : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Has_Vote : constant Boolean := Has_User_Vote (Uid, Tid);
   begin
      if Has_Vote then
         DBH.Handle.Execute
           ("DELETE FROM user_photo_of_the_week "
              & "WHERE user_login=" & Q (Uid)
              & " AND post_id=" & To_String  (Tid));
      else
         DBH.Handle.Execute
           ("INSERT INTO user_photo_of_the_week "
              & "VALUES (" & Q (Uid) & ", " & To_String (Tid) & ", 0)");
      end if;
   end Toggle_Vote_Week_Photo;

   -------------------
   -- Update_Rating --
   -------------------

   procedure Update_Rating
     (Uid      : in String;
      Tid      : in Id;
      Criteria : in String;
      Value    : in String)
   is
      SQL  : constant String :=
               "SELECT 1 FROM rating WHERE user_login="
                 & Q (Uid) & " AND post_id="
                 & To_String (Tid) & " AND criteria_id="
                 & Q (Criteria);
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         --  Need update
         Iter.End_Select;
         DBH.Handle.Execute
           ("UPDATE rating SET post_rating = " & Q (Value)
            & "WHERE user_login="
            & Q (Uid) & " AND post_id=" & To_String (Tid)
            & " AND criteria_id=" & Q (Criteria));
      else
         --  Insert new rating
         Iter.End_Select;
         DBH.Handle.Execute
           ("INSERT INTO rating VALUES (" & Q (Uid)
            & ", " & To_String (Tid) & ", " & Q (Criteria)
            & ", " & Q (Value) & ")");
      end if;
   end Update_Rating;

end V2P.Database.Vote;
