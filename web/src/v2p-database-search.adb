------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with V2P.DB_Handle;
with V2P.Settings;

with V2P.Template_Defs.Chunk_Search_User;
with V2P.Template_Defs.Chunk_Search_Comment;
with V2P.Template_Defs.Chunk_Threads_List;

package body V2P.Database.Search is

   use V2P.Template_Defs;
   use type Templates.Tag;

   function Pattern_DB
     (Field   : in String;
      Pattern : in Word_Set) return String;
   --  Returns the DB filter on Field to match the given patterns

   --------------
   -- Comments --
   --------------

   function Comments (Pattern : in Word_Set) return Templates.Translate_Set is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL     : constant String :=
                  "SELECT id, date, user_login, comment, post_id"
                  & " FROM comment, post_comment "
                  & " WHERE post_comment.comment_id=comment.id"
                  & " AND " & Pattern_DB ("comment", Pattern)
                  & " ORDER BY date DESC"
                  & " LIMIT " & Utils.Image (Settings.Max_Search_Results);
      Set     : Templates.Translate_Set;
      Iter    : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line    : DB.String_Vectors.Vector;
      Tid, Id : Templates.Tag;
      Date    : Templates.Tag;
      Login   : Templates.Tag;
      Comment : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Id      := Id & DB.String_Vectors.Element (Line, 1);
         Date    := Date & DB.String_Vectors.Element (Line, 2);
         Login   := Login & DB.String_Vectors.Element (Line, 3);
         Comment := Comment & DB.String_Vectors.Element (Line, 4);
         Tid     := Tid & DB.String_Vectors.Element (Line, 5);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_Comment.ID, Id));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_Comment.DATE, Date));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_Comment.USER_LOGIN, Login));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_Comment.COMMENT, Comment));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_Comment.COMMENT_TID, Tid));

      return Set;
   end Comments;

   ----------------
   -- Pattern_DB --
   ----------------

   function Pattern_DB
     (Field   : in String;
      Pattern : in Word_Set) return String
   is
      Result : Unbounded_String;
   begin
      for K in Pattern'Range loop
         if Result /= Null_Unbounded_String then
            Append (Result, " AND ");
         end if;

         Append (Result, Field & " LIKE '%" & To_String (Pattern (K)) & "%'");
      end loop;

      return To_String (Result);
   end Pattern_DB;

   -----------
   -- Posts --
   -----------

   function Posts (Pattern : in Word_Set) return Templates.Translate_Set is
      DBH       : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);
      SQL       : constant String := "SELECT post.name, post.id,"
                    & " user_post.user_login, category.name, comment_counter,"
                    & " visit_counter, "
                    & " (SELECT filename FROM photo WHERE id=post.photo_id),"
                    & " post.hidden"
                    & " FROM post, category, user_post, forum "
                    & " WHERE post.id=user_post.post_id"
                    & " AND post.category_id=category.id"
                    & " AND forum.for_photo='TRUE'"
                    & " AND forum.id=category.forum_id"
                    & " AND ((" & Pattern_DB ("post.name", Pattern) & ") "
                    & " OR (" & Pattern_DB ("post.comment", Pattern) & "))"
                    & " ORDER BY post.date_post DESC"
                    & " LIMIT " & Utils.Image (Settings.Max_Search_Results);
      Set       : Templates.Translate_Set;
      Iter      : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line      : DB.String_Vectors.Vector;
      Name      : Templates.Tag;
      Tid       : Templates.Tag;
      Owner     : Templates.Tag;
      Category  : Templates.Tag;
      Comment_C : Templates.Tag;
      Visit_C   : Templates.Tag;
      Thumb     : Templates.Tag;
      Hidden    : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Name      := Name      & DB.String_Vectors.Element (Line, 1);
         Tid       := Tid       & DB.String_Vectors.Element (Line, 2);
         Owner     := Owner     & DB.String_Vectors.Element (Line, 3);
         Category  := Category  & DB.String_Vectors.Element (Line, 4);
         Comment_C := Comment_C & DB.String_Vectors.Element (Line, 5);
         Visit_C   := Visit_C   & DB.String_Vectors.Element (Line, 6);
         Thumb     := Thumb     & DB.String_Vectors.Element (Line, 7);
         Hidden    := Hidden    & DB.String_Vectors.Element (Line, 8);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.TID, Tid));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.OWNER, Owner));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.CATEGORY, Category));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.VISIT_COUNTER, Visit_C));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.COMMENT_COUNTER, Comment_C));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.THUMB_SOURCE, Thumb));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.HIDDEN, Hidden));

      return Set;
   end Posts;

   ----------------
   -- Text_Posts --
   ----------------

   function Text_Posts
     (Pattern : in Word_Set) return Templates.Translate_Set
   is
      DBH       : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);
      SQL       : constant String := "SELECT post.name, post.id,"
                    & " user_post.user_login, category.name, comment_counter,"
                    & " visit_counter, post.hidden"
                    & " FROM post, category, user_post, forum "
                    & " WHERE post.id=user_post.post_id"
                    & " AND post.category_id=category.id"
                    & " AND forum.for_photo='FALSE'"
                    & " AND forum.id=category.forum_id"
                    & " AND ((" & Pattern_DB ("post.name", Pattern) & ") "
                    & " OR (" & Pattern_DB ("post.comment", Pattern) & "))"
                    & " ORDER BY post.date_post DESC"
                    & " LIMIT " & Utils.Image (Settings.Max_Search_Results);
      Set       : Templates.Translate_Set;
      Iter      : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line      : DB.String_Vectors.Vector;
      Name      : Templates.Tag;
      Tid       : Templates.Tag;
      Owner     : Templates.Tag;
      Category  : Templates.Tag;
      Comment_C : Templates.Tag;
      Visit_C   : Templates.Tag;
      Hidden    : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Name      := Name      & DB.String_Vectors.Element (Line, 1);
         Tid       := Tid       & DB.String_Vectors.Element (Line, 2);
         Owner     := Owner     & DB.String_Vectors.Element (Line, 3);
         Category  := Category  & DB.String_Vectors.Element (Line, 4);
         Comment_C := Comment_C & DB.String_Vectors.Element (Line, 5);
         Visit_C   := Visit_C   & DB.String_Vectors.Element (Line, 6);
         Hidden    := Hidden    & DB.String_Vectors.Element (Line, 7);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.NAME, Name));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.TID, Tid));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.OWNER, Owner));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.CATEGORY, Category));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.VISIT_COUNTER, Visit_C));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.COMMENT_COUNTER, Comment_C));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Threads_List.HIDDEN, Hidden));

      return Set;
   end Text_Posts;

   -----------
   -- Users --
   -----------

   function Users (Pattern : in Word_Set) return Templates.Translate_Set is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL     : constant String := "SELECT login, content FROM user, user_page"
                  & " WHERE user.login=user_page.user_login "
                  & " AND ((" & Pattern_DB ("user_page.content", Pattern)
                  & ") OR (" & Pattern_DB ("user.login", Pattern) & "))"
                  & " ORDER BY user.created DESC"
                  & " LIMIT " & Utils.Image (Settings.Max_Search_Results);
      Set     : Templates.Translate_Set;
      Iter    : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line    : DB.String_Vectors.Vector;
      Login   : Templates.Tag;
      Content : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Login   := Login & DB.String_Vectors.Element (Line, 1);
         Content := Content & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_User.LOGIN, Login));
      Templates.Insert
        (Set, Templates.Assoc (Chunk_Search_User.CONTENT, Content));

      return Set;
   end Users;

end V2P.Database.Search;
