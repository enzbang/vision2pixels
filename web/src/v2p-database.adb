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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with DB;

with V2P.DB_Handle;
with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Block_Login;

package body V2P.Database is

   use V2P.Template_Defs;

   procedure Connect;
   --  Connect to the database if needed

   DBH       : DB.Handle'Class := DB_Handle.Get;

   Connected : Boolean := False;

   -------------
   -- Connect --
   -------------

   procedure Connect is
   begin
      if not Connected then
         DBH.Connect ("../db/data/testing.db");
         Connected := True;
      end if;
   end Connect;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Id : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      Set      : Templates.Translate_Set;
      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
      User     : Templates.Tag;
      Date     : Templates.Tag;
      Comment  : Templates.Tag;
      Filename : Templates.Tag;
   begin
      Connect;

      --  Get image information

      DBH.Prepare_Select
        (Iter,
         "select photo.name, category.name, photo.filename"
         & " from photo, category "
         & " where photo.id='" & Id
         & "' and photo.category_id = category.id");

      if Iter.More then
         Iter.Get_Line (Line);

         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Name, DB.String_Vectors.Element (Line, 1)));
         Templates.Insert
           (Set, Templates.Assoc
              (Forum_Entry.Image_Source, DB.String_Vectors.Element (Line, 2)));
         Line.Clear;
      end if;

      Iter.End_Select;

      --  Get image's thread

      DBH.Prepare_Select
        (Iter,
         "select date, user_login, comment, filename"
         & " from comment, photo_comment"
         & " where photo_id='" & Id
         & "' and photo_comment.comment_id=comment.id");

      while Iter.More loop
         Iter.Get_Line (Line);

         Date     := Date     & DB.String_Vectors.Element (Line, 1);
         User     := User     & DB.String_Vectors.Element (Line, 2);
         Comment  := Comment  & DB.String_Vectors.Element (Line, 3);
         Filename := Filename & DB.String_Vectors.Element (Line, 4);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Date, Date));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.User, User));
      Templates.Insert (Set, Templates.Assoc (Forum_Entry.Comment, Comment));
--        Templates.Insert
--          (Set, Templates.Assoc (Forum_Entry.File_Attachment, Filename));

      return Set;
   end Get_Entry;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (User : in String) return String is
      use type Templates.Tag;

      Iter     : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line     : DB.String_Vectors.Vector;
   begin
      Connect;

      DBH.Prepare_Select
        (Iter, "select password from user where login='" & User & ''');

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

   function Get_Threads return Templates.Translate_Set is
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
         & " where photo.category_id = category.id");

      while Iter.More loop
         Iter.Get_Line (Line);

         Id       := Id       & DB.String_Vectors.Element (Line, 1);
         Name     := Name     & DB.String_Vectors.Element (Line, 2);
         Category := Category & DB.String_Vectors.Element (Line, 3);
         Counter  := Counter  & DB.String_Vectors.Element (Line, 4);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc (Forum_Threads.Id, Id));
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

   function Get_User (Id : in String) return Templates.Translate_Set is
      use type Templates.Tag;

      Set : Templates.Translate_Set;
   begin
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.Login, Id));
      Templates.Insert
        (Set, Templates.Assoc (Block_Login.Password, Get_Password (Id)));

      return Set;
   end Get_User;

end V2P.Database;
