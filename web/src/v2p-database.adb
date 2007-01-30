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
with Ada.Strings.Unbounded;

with DB;
with Settings;

with V2P.Web_Server;
with V2P.DB_Handle;
with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Block_Forum_Threads;
with V2P.Template_Defs.Block_Forum_Navigate;
with V2P.Template_Defs.Block_Forum_List;
with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.R_Block_Forum_List;

package body V2P.Database is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use V2P.Template_Defs;

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   Null_DBH : constant TLS_DBH := (null, False);

   package DBH_TLS is new Task_Attributes (TLS_DBH, Null_DBH);

   procedure Connect (DBH : in out TLS_DBH);
   --  Connect to the database if needed

   function Q (Str : in String) return String;
   pragma Inline (Q);
   --  Quote the string and double all single quote in Str to be able to insert
   --  a quote into the database.
   --  Returns Null if empty string

   function I (Int : in Integer) return String;
   pragma Inline (I);
   --  Returns Integer image

   function Threads_Ordered_Select
     (Fid        : in String := "";
      User       : in String := "";
      From       : in Natural := 0;
      Filter     : in Filter_Mode := All_Messages;
      Where_Cond : in String := "";
      Order_Dir  : in Order_Direction := DESC;
      Limit      : in Natural := 0)
      return Unbounded_String;
   --  Returns the select SQL query for listing threads with Filter

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in out TLS_DBH) is
   begin
      if not DBH.Connected then
         DBH.Handle := new DB.Handle'Class'(DB_Handle.Get);
         DBH.Handle.Connect (Settings.Get_DB_Name);
         DBH.Connected := True;
         DBH_TLS.Set_Value (DBH);
      end if;
   end Connect;

   --------------------
   -- Get_Categories --
   --------------------

   function Get_Categories (Fid : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH  : TLS_DBH := DBH_TLS.Value;
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Templates.Tag;
      Name : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select id, name from category" & " where forum_id=" & Q (Fid));

      while Iter.More loop
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.Category_Id, Id));
      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.Category, Name));

      return Set;
   end Get_Categories;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Tid : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH  : TLS_DBH := DBH_TLS.Value;
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

      while Iter.More loop
         --  ?? only one
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc (Block_New_Comment.Category_Id, Id));
      Templates.Insert
        (Set, Templates.Assoc (R_Block_Forum_List.Category, Name));

      return Set;
   end Get_Category;

   ----------------------------
   -- Get_Category_Full_Name --
   ----------------------------

   function Get_Category_Full_Name (CID : in String) return String is
      DBH  : TLS_DBH := DBH_TLS.Value;
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
              (DB.String_Vectors.Element (Line, 1),
               DB.String_Vectors.Element (Line, 2)));
         Line.Clear;
      end if;

      Iter.End_Select;

      return To_String (Name);
   end Get_Category_Full_Name;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Tid : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH                : TLS_DBH := DBH_TLS.Value;
      Set                : Templates.Translate_Set;
      Iter               : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line               : DB.String_Vectors.Vector;
      Comment_Id         : Templates.Tag;
      Comment_Level      : Templates.Tag;
      Nb_Levels_To_Close : Templates.Tag;
      User               : Templates.Tag;
      Anonymous          : Templates.Tag;
      Date               : Templates.Tag;
      Comment            : Templates.Tag;
      Filename           : Templates.Tag;

   begin
      Connect (DBH);

      --  Get thread information

      DBH.Handle.Prepare_Select
        (Iter, "select post.name, post.comment, "
         & "case post.photo_id is null "
         & "when 1 then NULL else photo.filename end "
         & "from post, photo where "
         & "((post.photo_id NOTNULL and photo.id = post.photo_id) "
         & "or (post.photo_id ISNULL)) "
         & "and post.id=" & Q (Tid));

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Name, DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Comment,
               DB.String_Vectors.Element (Line, 2)));

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Source_Prefix,
               V2P.Web_Server.Images_Source_Prefix));

         --  Insert the image path

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Source,
               DB.String_Vectors.Element (Line, 3)));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Get threads

      DBH.Handle.Prepare_Select
        (Iter,
         "select comment.id, date, user_login, anonymous_user, "
         & "comment, filename"
         & " from comment, post_comment"
         & " where post_id=" & Q (Tid)
         & " and post_comment.comment_id=comment.id");

      while Iter.More loop
         Iter.Get_Line (Line);

         Comment_Id := Comment_Id & DB.String_Vectors.Element (Line, 1);
         Date       := Date       & DB.String_Vectors.Element (Line, 2);
         User       := User       & DB.String_Vectors.Element (Line, 3);
         Anonymous  := Anonymous  & DB.String_Vectors.Element (Line, 4);
         Comment    := Comment    & DB.String_Vectors.Element (Line, 5);
         Filename   := Filename   & DB.String_Vectors.Element (Line, 6);

         --  Unthreaded view

         Comment_Level      := Comment_Level      & 1;
         Nb_Levels_To_Close := Nb_Levels_To_Close & 1;

         Line.Clear;
      end loop;

      Iter.End_Select;

      --    Templates.Insert
      --      (Set, Templates.Assoc (Forum_Entry.Comment_Id, Comment_Id));
      --    Comment id is not active since thread view is not supported
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Date, Date));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.User, User));
      Templates.Insert
        (Set, Templates.Assoc (Forum_Entry.Anonymous_User, Anonymous));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Comment, Comment));
      Templates.Insert
        (Set, Templates.Assoc (Forum_Entry.Comment_Level, Comment_Level));
      Templates.Insert
        (Set,
         Templates.Assoc (Forum_Entry.Nb_Levels_To_Close, Nb_Levels_To_Close));

      return Set;
   end Get_Entry;

   ---------------
   -- Get_Forum --
   ---------------

   function Get_Forum (Fid : in String) return String is
      DBH  : TLS_DBH := DBH_TLS.Value;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select name from forum where id=" & Q (Fid));

      if Iter.More then
         Iter.Get_Line (Line);

         declare
            Name : constant String := DB.String_Vectors.Element (Line, 1);
         begin
            Line.Clear;
            Iter.End_Select;
            return Name;
         end;

      else
         Iter.End_Select;
         return "";
      end if;
   end Get_Forum;

   ----------------
   -- Get_Forums --
   ----------------

   function Get_Forums return Templates.Translate_Set is
      use type Templates.Tag;

      DBH  : TLS_DBH := DBH_TLS.Value;
      Set  : Templates.Translate_Set;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Templates.Tag;
      Name : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, "select id, name from forum");

      while Iter.More loop
         Iter.Get_Line (Line);

         Id   := Id & DB.String_Vectors.Element (Line, 1);
         Name := Name & DB.String_Vectors.Element (Line, 2);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Block_Forum_List.Fid, Id));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_List.Forum_Name, Name));

      return Set;
   end Get_Forums;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (Uid : in String) return String is
      use type Templates.Tag;

      DBH  : TLS_DBH := DBH_TLS.Value;
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      if Uid = "" then
         return "";
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
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

   ---------------------------------
   -- Get_Thread_Navigation_Links --
   ---------------------------------

   function Get_Thread_Navigation_Links
     (Fid       : in String;
      Tid       : in String;
      User      : in String := "";
      From      : in Natural := 0;
      Filter    : in Filter_Mode := All_Messages;
      Order_Dir : in Order_Direction := DESC)
      return Templates.Translate_Set
   is
      Post_Date : constant String :=
        "(select date_post from post where id = " & Tid & ") ";

      And_Date_Post : constant String := " and date_post ";

      DBH           : TLS_DBH := DBH_TLS.Value;
      Set           : Templates.Translate_Set;
      Iter          : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line          : DB.String_Vectors.Vector;
      Select_Stmt   : Unbounded_String;

   begin
      if Order_Dir = DESC then
         --  Next is previous

         Select_Stmt := Threads_Ordered_Select
           (Fid, User, From, Filter,
            And_Date_Post & " > " & Post_Date,
            ASC, 1);
      else
         Select_Stmt := Threads_Ordered_Select
           (Fid, User, From, Filter,
            And_Date_Post & " < " & Post_Date,
            DESC, 1);
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, To_String (Select_Stmt));

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Previous,
               DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Previous_Thumb,
               DB.String_Vectors.Element (Line, 3)));

         Line.Clear;
      end if;

      Iter.End_Select;


      if Order_Dir = DESC then
         --  Previous is next

         Select_Stmt := Threads_Ordered_Select
           (Fid, User, From, Filter,
            And_Date_Post & " < " & Post_Date,
            DESC, 1);
      else
         Select_Stmt := Threads_Ordered_Select
           (Fid, User, From, Filter,
            And_Date_Post & " > " & Post_Date,
            ASC, 1);
      end if;

      DBH.Handle.Prepare_Select (Iter, To_String (Select_Stmt));

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Next,
               DB.String_Vectors.Element (Line, 1)));

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Next_Thumb,
               DB.String_Vectors.Element (Line, 3)));

         Line.Clear;
      end if;

      Iter.End_Select;

      return Set;
   end Get_Thread_Navigation_Links;

   -----------------
   -- Get_Threads --
   -----------------

   function Get_Threads
     (Fid       : in String := "";
      User      : in String := "";
      From      : in Natural := 0;
      Filter    : in Filter_Mode := All_Messages;
      Order_Dir : in Order_Direction := DESC)
      return Templates.Translate_Set
   is
      use type Templates.Tag;

      DBH             : TLS_DBH := DBH_TLS.Value;
      Set             : Templates.Translate_Set;
      Iter            : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line            : DB.String_Vectors.Vector;
      Id              : Templates.Tag;
      Name            : Templates.Tag;
      Category        : Templates.Tag;
      Comment_Counter : Templates.Tag;
      Visit_Counter   : Templates.Tag;
      Thumb           : Templates.Tag;
      Select_Stmt     : Unbounded_String;

   begin
      Connect (DBH);

      Select_Stmt := Threads_Ordered_Select
        (Fid => Fid,
         User => User,
         From => From,
         Filter => Filter,
         Order_Dir => Order_Dir);

      if Filter = Fifty_Messages then
         --  ???

         --  Add next and previous information into the translate set

         if From /= 0 then
            Templates.Insert
              (Set, Templates.Assoc
                 (Block_Forum_Navigate.Previous, From - 50));
         end if;

         --  ??? need to check if there is more data !
         Templates.Insert
           (Set, Templates.Assoc
              (Block_Forum_Navigate.Next, From + 50));
      end if;

      DBH.Handle.Prepare_Select (Iter, To_String (Select_Stmt));

      while Iter.More loop
         Iter.Get_Line (Line);

         Id              := Id       & DB.String_Vectors.Element (Line, 1);
         Name            := Name     & DB.String_Vectors.Element (Line, 2);
         Thumb           := Thumb    & DB.String_Vectors.Element (Line, 3);
         Category        := Category & DB.String_Vectors.Element (Line, 4);
         Comment_Counter := Comment_Counter
           & DB.String_Vectors.Element (Line, 5);
         Visit_Counter   := Visit_Counter
           & DB.String_Vectors.Element (Line, 6);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set, Templates.Assoc
           (Block_Forum_Threads.Thumb_Source, Thumb));

      Templates.Insert (Set, Templates.Assoc (Block_Forum_Threads.Tid, Id));
      Templates.Insert (Set, Templates.Assoc (Block_Forum_Threads.Name, Name));
      Templates.Insert
        (Set, Templates.Assoc (Block_Forum_Threads.Category, Category));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Forum_Threads.Comment_Counter, Comment_Counter));
      Templates.Insert
        (Set, Templates.Assoc
           (Block_Forum_Threads.Visit_Counter, Visit_Counter));
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
      DBH : TLS_DBH := DBH_TLS.Value;
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

   procedure Insert_Comment
     (Uid       : in String;
      Anonymous : in String;
      Thread    : in String;
      Name      : in String;
      Comment   : in String;
      Filename  : in String)
   is
      pragma Unreferenced (Name);

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String);
      --  Insert row into Comment table

      procedure Insert_Table_post_Comment (post_Id, Comment_Id : in String);
      --  Insert row into post_Comment table

      DBH : TLS_DBH := DBH_TLS.Value;

      --------------------------
      -- Insert_Table_Comment --
      --------------------------

      procedure Insert_Table_Comment
        (User_Login, Anonymous, Comment : in String)
      is
         SQL : constant String :=
                 "insert into comment ('user_login', 'anonymous_user', "
                   & "'comment', 'filename')"
                   & " values ("
                   & Q (User_Login) & ',' & Q (Anonymous) & ',' & Q (Comment)
                   & ',' & Q (Filename) & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_Comment;

      --------------------------------
      -- Insert_Table_post_Comment --
      --------------------------------

      procedure Insert_Table_post_Comment
        (post_Id, Comment_Id : in String)
      is
         SQL : constant String :=
                 "insert into post_comment values ("
                   & post_Id & "," & Comment_Id & ')';
      begin
         DBH.Handle.Execute (SQL);
      end Insert_Table_post_Comment;

   begin
      Connect (DBH);
      DBH.Handle.Begin_Transaction;
      Insert_Table_Comment (Uid, Anonymous, Comment);
      Insert_Table_post_Comment (Thread, DBH.Handle.Last_Insert_Rowid);
      DBH.Handle.Commit;
   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
   end Insert_Comment;

   -----------------
   -- Insert_Post --
   -----------------

   procedure Insert_Post
     (Uid         : in String;
      Category_Id : in String;
      Name        : in String;
      Comment     : in String;
      Filename    : in String  := "";
      Height      : in Integer := 0;
      Width       : in Integer := 0;
      Size        : in Integer := 0)
   is

      procedure Insert_Table_Photo
        (Filename : in String;
         Height   : in Integer;
         Width    : in Integer;
         Size     : in Integer);
      --  Insert row into the photo table

      procedure Insert_Table_Post
        (Name, Category_Id, Comment, Photo_Id : in String);
      --  Insert row into the post table

      procedure Insert_Table_User_Post (Uid, Post_Id : in String);
      --  Insert row into the user_post table

      DBH : TLS_DBH := DBH_TLS.Value;

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

      if Filename /= "" then
         Insert_Table_Photo (Filename, Height, Width, Size);
         Insert_Table_Post
           (Name, Category_Id, Comment, DBH.Handle.Last_Insert_Rowid);
      else
         Insert_Table_Post (Name, Category_Id, Comment, "Null");
      end if;

      Insert_Table_User_Post (Uid, DBH.Handle.Last_Insert_Rowid);
      DBH.Handle.Commit;
   exception
      when E : DB.DB_Error =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
      when E : others =>
         DBH.Handle.Rollback;
         Text_IO.Put_Line (Exception_Message (E));
   end Insert_Post;

   ---------------
   -- Is_Author --
   ---------------

   function Is_Author (Uid, Pid : in String) return Boolean is
      DBH    : TLS_DBH := DBH_TLS.Value;
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

   ----------------------------
   -- Threads_Ordered_Select --
   ----------------------------

   function Threads_Ordered_Select
     (Fid        : in String  := "";
      User       : in String  := "";
      From       : in Natural := 0;
      Filter     : in Filter_Mode := All_Messages;
      Where_Cond : in String  := "";
      Order_Dir  : in Order_Direction := DESC;
      Limit      : in Natural := 0)
     return Unbounded_String
   is
      SQL_Select : constant String :=
        "select post.id, post.name, case post.photo_id is null "
        & "when 1 then NULL else photo.filename end, "
                       & "category.name, comment_counter, visit_counter ";
      SQL_From   : constant String := " from post, category, photo ";
      SQL_Where  : constant String := " where post.category_id = category.id "
        & " and ((photo_id NOTNULL and photo.id = post.photo_id) "
        & "or (photo_id ISNULL)) " & Where_Cond;
      Ordering   : constant String := " order by post.date_post " &
        Order_Direction'Image (Order_Dir);

      Select_Stmt : Unbounded_String := To_Unbounded_String ("");
   begin
      if User /= "" and then Fid /= "" then
         --  ???

         Select_Stmt := Select_Stmt & SQL_Select & SQL_From & ", user_post"
           & SQL_Where
           & "and category.forum_id = " & Q (Fid)
           & "and user_post.post_id = post.id"
           & "and user_post.user_id = " & Q (User);

      elsif User /= "" and then Fid = "" then
         --  ???

         Select_Stmt := Select_Stmt & SQL_Select & SQL_From & ", user_post "
           & SQL_Where
           & " and user_post.post_id = post.id "
           & " and user_post.user_login = " & Q (User);

      else
         --  Anonymous login

         Select_Stmt := Select_Stmt & SQL_Select & SQL_From
           & SQL_Where
           & " and category.forum_id = " & Q (Fid);
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
            Select_Stmt := Select_Stmt
              & Ordering;
            if Limit = 0 then
               Select_Stmt := Select_Stmt
                 & " limit 50 offset" & Positive'Image (From);
            end if;
         when All_Messages =>
            Select_Stmt := Select_Stmt
              & Ordering;
      end case;

      if Limit /= 0 then
         Select_Stmt := Select_Stmt & " limit " & Natural'Image (Limit);
      end if;

      return Select_Stmt;
   end Threads_Ordered_Select;

end V2P.Database;
