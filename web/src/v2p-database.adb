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

with Ada.Exceptions;
with Ada.Text_IO;

with DB;
with Settings;

with V2P.Web_Server;
with V2P.DB_Handle;
with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Login;

package body V2P.Database is

   use Ada;
   use Ada.Exceptions;

   use V2P.Template_Defs;

   procedure Connect;
   --  Connect to the database if needed

   DBH       : DB.Handle'Class := DB_Handle.Get;

   Connected : Boolean := False;

   function Q (Str : in String) return String;
   pragma Inline (Q);
   --  Quote the string

   -------------
   -- Connect --
   -------------

   procedure Connect is
   begin
      if not Connected then
         DBH.Connect (Settings.Get_DB_Name);
         Connected := True;
      end if;
   end Connect;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Tid : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      Set                : Templates.Translate_Set;
      Iter               : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line               : DB.String_Vectors.Vector;
      Comment_Id         : Templates.Tag;
      Comment_Level      : Templates.Tag;
      Nb_Levels_To_Close : Templates.Tag;
      User               : Templates.Tag;
      Date               : Templates.Tag;
      Comment            : Templates.Tag;
      Filename           : Templates.Tag;
   begin
      Connect;

      --  Get image information

      DBH.Prepare_Select
        (Iter,
         "select photo.name, category.name, photo.filename"
         & " from photo, category "
         & " where photo.id=" & Q (Tid)
         & " and photo.category_id = category.id");

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Name, DB.String_Vectors.Element (Line, 1)));

         --  Insert the image path

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Source_Prefix,
               V2P.Web_Server.Image_Source_Prefix));

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Source, DB.String_Vectors.Element (Line, 3)));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Get image's thread

      DBH.Prepare_Select
        (Iter,
         "select comment.id, date, user_login, comment, filename"
         & " from comment, photo_comment"
         & " where photo_id=" & Q (Tid)
         & " and photo_comment.comment_id=comment.id");

      while Iter.More loop
         Iter.Get_Line (Line);

         Comment_Id := Comment_Id & DB.String_Vectors.Element (Line, 1);
         Date       := Date       & DB.String_Vectors.Element (Line, 2);
         User       := User       & DB.String_Vectors.Element (Line, 3);
         Comment    := Comment    & DB.String_Vectors.Element (Line, 4);
         Filename   := Filename   & DB.String_Vectors.Element (Line, 5);

         --  Unthreaded view

         Comment_Level      := Comment_Level      & 1;
         Nb_Levels_To_Close := Nb_Levels_To_Close & 1;

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Forum_Entry.Comment_Id, Comment_Id));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Date, Date));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.User, User));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Comment, Comment));
      Templates.Insert
        (Set, Templates.Assoc (Forum_Entry.Comment_Level, Comment_Level));
      Templates.Insert
        (Set, Templates.Assoc
           (Forum_Entry.Nb_Levels_To_Close, Nb_Levels_To_Close));
--        Templates.Insert
--          (Set, Templates.Assoc (Forum_Entry.File_Attachment, Filename));

      return Set;
   end Get_Entry;

   ----------------
   -- Get_Forums --
   ----------------

   function Get_Forums return Templates.Translate_Set is
      use type Templates.Tag;

      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Templates.Tag;
      Name : Templates.Tag;
   begin
      Connect;

      DBH.Prepare_Select (Iter, "select id, name from forum");

      while Iter.More loop
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Forum_List.Fid, Id));
      Templates.Insert (Set, Templates.Assoc (Block_Forum_List.Name, Name));

      return Set;
   end Get_Forums;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (Uid : in String) return String is
      use type Templates.Tag;

      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect;

      DBH.Prepare_Select
        (Iter, "select password from user where login=" & Q (Uid));

      if Iter.More then
         Iter.Get_Line (Line);

         declare
            Password : constant String := DB.String_Vectors.Element (Line, 1);
         begin
            Line.Clear;
            return Password;
         end;

      else
         return "";
      end if;
   end Get_Password;

   -----------------
   -- Get_Threads --
   -----------------

   function Get_Threads (Fid : in String) return Templates.Translate_Set is
      use type Templates.Tag;

      Set      : Templates.Translate_Set;
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      Id       : Templates.Tag;
      Name     : Templates.Tag;
      Category : Templates.Tag;
      Counter  : Templates.Tag;
   begin
      Connect;

      DBH.Prepare_Select
        (Iter,
         "select photo.id, photo.name, category.name, comment_counter"
         & " from photo, category"
         & " where photo.category_id = category.id"
         & " and category.forum_id = " & Q (Fid));

      while Iter.More loop
         Iter.Get_Line (Line);

         Id       := Id       & DB.String_Vectors.Element (Line, 1);
         Name     := Name     & DB.String_Vectors.Element (Line, 2);
         Category := Category & DB.String_Vectors.Element (Line, 3);
         Counter  := Counter  & DB.String_Vectors.Element (Line, 4);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Forum_Threads.Tid, Id));
      Templates.Insert (Set, Templates.Assoc (Forum_Threads.Name, Name));
      Templates.Insert
        (Set, Templates.Assoc (Forum_Threads.Category, Category));
      Templates.Insert
        (Set, Templates.Assoc (Forum_Threads.Comment_Counter, Counter));

      return Set;
   end Get_Threads;

   --------------
   -- Get_User --
   --------------

   function Get_User (Uid : in String) return Templates.Translate_Set is
      use type Templates.Tag;

      Set : Templates.Translate_Set;
   begin
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.Login, Uid));
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.HTTP.Password, Get_Password (Uid)));

      return Set;
   end Get_User;

   --------------------
   -- Insert_Comment --
   --------------------

   procedure Insert_Comment
     (Uid     : in String;
      Forum   : in String;
      Thread  : in String;
      Name    : in String;
      Comment : in String)
   is
      pragma Unreferenced (Forum);
      pragma Unreferenced (Name);

      procedure Insert_Table_Comment (User_Login, Comment : in String);
      --  Insert row into Comment table

      procedure Insert_Table_Photo_Comment (Photo_Id, Comment_Id : in String);
      --  Insert row into Photo_Comment table

      --------------------------
      -- Insert_Table_Comment --
      --------------------------

      procedure Insert_Table_Comment (User_Login, Comment : in String) is
         SQL : constant String :=
                 "insert into comment ('user_login', 'comment') values ("
                   & Q (User_Login) & ',' & Q (Comment) & ')';
      begin
         DBH.Execute (SQL);
      end Insert_Table_Comment;

      --------------------------------
      -- Insert_Table_Photo_Comment --
      --------------------------------

      procedure Insert_Table_Photo_Comment
        (Photo_Id, Comment_Id : in String)
      is
         SQL : constant String :=
                 "insert into photo_comment values ("
                   & Photo_Id & "," & Comment_Id & ')';
      begin
         DBH.Execute (SQL);
      end Insert_Table_Photo_Comment;

   begin
      DBH.Begin_Transaction;
      Insert_Table_Comment (Uid, Comment);
      Insert_Table_Photo_Comment (Thread, DBH.Last_Insert_Rowid);
      DBH.Commit;
   exception
      when E : DB.DB_Error =>
         DBH.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
   end Insert_Comment;

   -------
   -- Q --
   -------

   function Q (Str : in String) return String is
   begin
      return ''' & Str & ''';
   end Q;

end V2P.Database;
