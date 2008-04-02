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

with V2P.DB_Handle;

with V2P.Template_Defs.Chunk_Search_User;

package body V2P.Database.Search is

   use V2P.Template_Defs;
   use type Templates.Tag;

   function Pattern_DB
     (Field   : in String;
      Pattern : in Word_Set) return String;
   --  Returns the DB filter on Field to match the given patterns

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
   -- Users --
   -----------

   function Users (Pattern : in Word_Set) return Templates.Translate_Set is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL     : constant String := "SELECT login, content FROM user, user_page"
                  & " WHERE user.login = user_page.user_login "
                  & " AND ((" & Pattern_DB ("user_page.content", Pattern)
                  & ") OR (" & Pattern_DB ("user.login", Pattern) & "))"
                  & " ORDER BY user.created DESC";
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
