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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Float_Text_IO;
with Ada.Strings.Hash;

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
with V2P.Template_Defs.Page_Week_Votes;
with V2P.Template_Defs.Set_Global;

package body V2P.Database.Vote is

   use Ada;

   use V2P.Database.Support;
   use V2P.Template_Defs;

   Current_CdC_Score : Float;
   CdC_Score_Ok      : Boolean := False;

   Weight : array (1 .. 7) of Float;
   --  Scores weight given the number of vote per user
   W_Initialized : Boolean := False;

   procedure Initialize_Weights;
   --  Initialize the Weight array above.
   --  Note that this can't be called from the elaboration as this will create
   --  an elaboration circuitry. It must be called in any routine using the
   --  Weight array above. Note also that it is safe even if called from
   --  multiple task, so this routine is not protected for this.

   package String_Vectors is
     new Containers.Indefinite_Vectors (Positive, String);

   type Name_Vect is record
      Filename : Unbounded_String;
      Users    : String_Vectors.Vector;
   end record;

   package Photo_Set is new Containers.Indefinite_Ordered_Maps
     (Id, Name_Vect, "<", "=");

   package Vote_Voter is new Containers.Indefinite_Hashed_Maps
     (String, Natural, Strings.Hash, "=", "=");

   type Votes_Data is record
      Photos      : Photo_Set.Map;
      Voter_Count : Vote_Voter.Map;
   end record;

   function Get_Week_Votes (Week_Id : Natural) return Votes_Data;

   -------------
   -- Get_CdC --
   -------------

   function Get_CdC (From : in Positive) return Templates.Translate_Set is
      DBH         : constant TLS_DBH_Access :=
                      TLS_DBH_Access (DBH_TLS.Reference);
      SQL         : constant String :=
                      "SELECT q.post_id, p.filename, q.elected_on, "
                        & "o.comment_counter, o.visit_counter, c.name, "
                        & "o.name, q.id "
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
      CdC_Id      : Templates.Tag;
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
         Templates.Append (Names, DB.String_Vectors.Element (Line, 7));
         Templates.Append (CdC_Id, DB.String_Vectors.Element (Line, 8));
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
        (Set, Templates.Assoc (Template_Defs.Block_Cdc.CDC_ID, CdC_Id));

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

   ------------------------
   -- Get_CdC_Best_Score --
   ------------------------

   function Get_CdC_Best_Score return Float is
   begin
      if not CdC_Score_Ok then
         Initialize_Weights;

         Current_CdC_Score := 0.0;

         declare
            Data : constant Votes_Data := Get_Week_Votes (Week_Id => 0);

            procedure Check_Photos (Pos : in Photo_Set.Cursor);
            --  Compute the CdC score for the pointed photo

            ------------------
            -- Check_Photos --
            ------------------

            procedure Check_Photos (Pos : in Photo_Set.Cursor) is

               procedure Check_Voters (Pos : in String_Vectors.Cursor);
               --  Handle voters and computer score for corresponding photo

               P_Score : Float := 0.0;

               ------------------
               -- Check_Voters --
               ------------------

               procedure Check_Voters (Pos : in String_Vectors.Cursor) is
                  Voter : constant String := String_Vectors.Element (Pos);
               begin
                  --  Compute score

                  declare
                     NV : constant Natural := Data.Voter_Count.Element (Voter);
                  begin
                     --  In the past we were not checking for the maximum of 7
                     --  votes, so it may have happened that a user had voted
                     --  for more than 7 photos. The script was then not
                     --  counting thoses votes.
                     if NV <= Weight'Last then
                        P_Score := P_Score + Weight (NV);
                     end if;
                  end;
               end Check_Voters;

            begin
               --  Check voters

               Photo_Set.Element (Pos).Users.Iterate (Check_Voters'Access);

               Current_CdC_Score := Float'Max (Current_CdC_Score, P_Score);
            end Check_Photos;

         begin
            Data.Photos.Iterate (Check_Photos'Access);
         end;

         CdC_Score_Ok := True;
      end if;

      return Current_CdC_Score;
   end Get_CdC_Best_Score;

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

   --------------------
   -- Get_Week_Votes --
   --------------------

   function Get_Week_Votes (Week_Id : Natural) return Votes_Data is

      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter    : DB.Iterator'Class := DB_Handle.Get_Iterator;
      SQL     : constant String :=
                  "SELECT user_login, post_id, "
                    & "(SELECT filename FROM photo, post"
                    & " WHERE post.photo_id=photo.id AND post_id=post.id) "
                    & "FROM user_photo_of_the_week "
                    & "WHERE week_id=" & To_String (Week_Id);
      Line    : DB.String_Vectors.Vector;

      Result  : Votes_Data;
      C_User  : Vote_Voter.Cursor;
      C_Photo : Photo_Set.Cursor;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         declare
            User : constant String := DB.String_Vectors.Element (Line, 1);
            Pid  : constant Id :=
                     Natural'Value (DB.String_Vectors.Element (Line, 2));
         begin
            --  Count number of votes for a given user

            C_User := Result.Voter_Count.Find  (User);

            if Vote_Voter.Has_Element (C_User) then
               Result.Voter_Count.Replace_Element
                 (C_User, Vote_Voter.Element (C_User) + 1);
            else
               Result.Voter_Count.Insert (User, 1);
            end if;

            --  Record vote for this photo

            C_Photo := Result.Photos.Find (Pid);

            if Photo_Set.Has_Element (C_Photo) then
               declare
                  procedure Add
                    (Key : in     Id;
                     NV  : in out Name_Vect);
                  --  Add user into the vector

                  ---------
                  -- Add --
                  ---------

                  procedure Add
                    (Key : in     Id;
                     NV  : in out Name_Vect)
                  is
                     pragma Unreferenced (Key);
                  begin
                     NV.Users.Append (User);
                  end Add;

               begin
                  Result.Photos.Update_Element (C_Photo, Add'Access);
               end;

            else
               Result.Photos.Insert
                 (Pid,
                  Name_Vect'(
                    To_Unbounded_String (DB.String_Vectors.Element (Line, 3)),
                    String_Vectors.To_Vector (User, 1)));
            end if;
         end;
      end loop;

      Iter.End_Select;

      return Result;
   end Get_Week_Votes;

   function Get_Week_Votes
     (Week_Id : Positive) return Templates.Translate_Set
   is
      use type AWS.Templates.Tag;

      procedure Add_Voters (Pos : in Vote_Voter.Cursor);
      --  Handle voters and corresponding count

      procedure Add_Photos (Pos : in Photo_Set.Cursor);
      --  Handle photos and voters

      type P_Data is record
         Score    : Float := 0.0;
         Pid      : Id;
         Filename : Unbounded_String;
         V        : Templates.Tag;
      end record;

      function "<" (Left, Right : P_Data) return Boolean;

      function "=" (Left, Right : P_Data) return Boolean;

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : P_Data) return Boolean is
      begin
         return Left.Score > Right.Score
           or else Left.Pid < Right.Pid;
      end "<";

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : P_Data) return Boolean is
      begin
         return Left.Pid = Right.Pid;
      end "=";

      package P_Set is new Ada.Containers.Ordered_Sets (P_Data, "<", "=");

      procedure Fill_Tag (Pos : in P_Set.Cursor);
      --  Fill sorted value into the corresponding tags

      D_Set    : P_Set.Set;

      Data      : constant Votes_Data := Get_Week_Votes (Week_Id);
      Set       : Templates.Translate_Set;
      Voters    : Templates.Tag;
      Votes     : Templates.Tag;
      Photos    : Templates.Tag;
      Filenames : Templates.Tag;
      Scores    : Templates.Tag;
      P_Voters  : Templates.Tag;

      ----------------
      -- Add_Photos --
      ----------------

      procedure Add_Photos (Pos : in Photo_Set.Cursor) is

         procedure Add_Voters (Pos : in String_Vectors.Cursor);
         --  Handle voter names

         D : P_Data;

         ----------------
         -- Add_Voters --
         ----------------

         procedure Add_Voters (Pos : in String_Vectors.Cursor) is
            Voter : constant String := String_Vectors.Element (Pos);
         begin
            D.V := D.V & Voter;

            --  Compute score

            declare
               NV : constant Natural := Data.Voter_Count.Element (Voter);
            begin
               --  In the past we were not checking for the maximum of 7 votes,
               --  so it may have happened that a user had voted for more than
               --  7 photos. The script was then not counting thoses votes.
               if NV <= Weight'Last then
                  D.Score := D.Score + Weight (NV);
               end if;
            end;
         end Add_Voters;

      begin
         D.Pid := Photo_Set.Key (Pos);
         D.Filename := Photo_Set.Element (Pos).Filename;

         --  Add each voters

         Photo_Set.Element (Pos).Users.Iterate (Add_Voters'Access);

         D_Set.Insert (D);
      end Add_Photos;

      ----------------
      -- Add_Voters --
      ----------------

      procedure Add_Voters (Pos : in Vote_Voter.Cursor) is
      begin
         Voters := Voters & Vote_Voter.Key (Pos);
         Votes  := Votes & Vote_Voter.Element (Pos);
      end Add_Voters;

      --------------
      -- Fill_Tag --
      --------------

      procedure Fill_Tag (Pos : in P_Set.Cursor) is
         D : constant P_Data := P_Set.Element (Pos);
      begin
         Photos := Photos & D.Pid;
         Filenames := Filenames & D.Filename;
         P_Voters := P_Voters & D.V;

         declare
            Buffer : String (1 .. 10);
         begin
            Float_Text_IO.Put (Buffer, D.Score, Aft => 2, Exp => 0);

            Scores := Scores & Buffer;
         end;
      end Fill_Tag;

   begin
      Initialize_Weights;

      Data.Voter_Count.Iterate (Add_Voters'Access);
      Data.Photos.Iterate (Add_Photos'Access);

      D_Set.Iterate (Fill_Tag'Access);

      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Week_Votes.VOTERS, Voters));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Week_Votes.NB_VOTES, Votes));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Week_Votes.PHOTOS, Photos));
      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Page_Week_Votes.FILENAMES, Filenames));
      Templates.Insert
        (Set, Templates.Assoc (Template_Defs.Page_Week_Votes.SCORES, Scores));
      Templates.Insert
        (Set, Templates.Assoc
           (Template_Defs.Page_Week_Votes.PHOTO_VOTERS, P_Voters));
      return Set;
   end Get_Week_Votes;

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

   ------------------------
   -- Initialize_Weights --
   ------------------------

   procedure Initialize_Weights is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL  : constant String := "SELECT * FROM vote_ponderated";
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      K    : Positive := Weight'First;
   begin
      if not W_Initialized then
         Connect (DBH);

         DBH.Handle.Prepare_Select (Iter, SQL);

         while Iter.More loop
            Iter.Get_Line (Line);

            Weight (K) := Float'Value (DB.String_Vectors.Element (Line, 1));
            K := K + 1;
            exit when K > Weight'Last;
         end loop;

         Iter.End_Select;

         W_Initialized := True;
      end if;
   end Initialize_Weights;

   -------------
   -- Nb_Vote --
   -------------

   function Nb_Vote (Uid : in String) return Natural is
      DBH    : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL    : constant String :=
                 "SELECT COUNT(*) FROM user_photo_of_the_week "
                   & "WHERE user_login=" & Q (Uid)
                   & " AND week_id=0";
      --  week_id=0 as we want only the photo for which the user has voted for
      --  the current open vote.
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line   : DB.String_Vectors.Vector;
      Result : Natural := 0;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      if Iter.More then
         Iter.Get_Line (Line);
         Result := Natural'Value (DB.String_Vectors.Element (Line, 1));
      end if;

      Iter.End_Select;

      return Result;
   end Nb_Vote;

   ----------------------------
   -- Toggle_Vote_Week_Photo --
   ----------------------------

   procedure Toggle_Vote_Week_Photo (Uid : in String; Tid : in Id) is
      DBH      : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Has_Vote : constant Boolean := Has_User_Vote (Uid, Tid);
   begin
      Connect (DBH);

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

      --  Invalidate CdC score

      CdC_Score_Ok := False;
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
