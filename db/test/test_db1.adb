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

with Ada.Text_IO;

pragma Warnings (Off);
with DB.SQLite;

procedure Test_DB1 is

   use Ada;

   H   : DB.SQLite.Handle;
   I   : DB.SQLite.Iterator;
   Res : DB.String_Vectors.Vector;

   procedure Print (Position : DB.String_Vectors.Cursor);
   --  Print an item

   procedure Print (Position : DB.String_Vectors.Cursor) is
   begin
      Text_IO.Put (DB.String_Vectors.Element (Position) & " ");
   end Print;

begin
   DB.SQLite.Connect (H, "../data/testing.db");

   begin
      DB.SQLite.Execute
        (H, "insert into user values ('toto', 'pwd', 'toto@here.com')");
   exception
      when others =>
         --  Catch all exceptions, just in case the data have already been
         --  inserted into the database.
         null;
   end;

   DB.SQLite.Prepare_Select (H, I, "select * from user;");

   while DB.SQLite.More (I) loop
      DB.SQLite.Get_Line (I, Res);
      DB.String_Vectors.Iterate (Res, Print'Access);
      DB.String_Vectors.Clear (Res);
      Text_IO.New_Line;
   end loop;

   DB.SQLite.End_Select (I);

   DB.SQLite.Close (H);
end Test_DB1;
