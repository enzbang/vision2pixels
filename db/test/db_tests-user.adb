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

with Ada.Strings.Unbounded;

with DB.SQLite;

package body DB_Tests.User is

   use Ada;
   use Ada.Strings.Unbounded;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;

   procedure Simple_User (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Simple User table test

   function "+"
     (Str : in String) return Unbounded_String
      renames To_Unbounded_String;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return New_String ("Simple user table test");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Simple_User'Access, "Simple User test");
   end Register_Tests;

   -----------------
   -- Simple_User --
   -----------------

   procedure Simple_User (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      type User is array (Positive range 1 .. 3) of Unbounded_String;
      type User_Set is array (Positive range <>) of User;

      Expected : constant User_Set :=
                   User_Set'
                     (1 => User'(+"enzbang", +"password", +"v2p@ramonat.fr"),
                      2 => User'(+"test", +"test", +"test@whatever.fr"),
                      3 => User'(+"toto", +"pwd", +"toto@here.com"),
                      4 => User'(+"turbo", +"turbopass", +"v2p@obry.net"));

      H        : DB.SQLite.Handle;
      I        : DB.SQLite.Iterator;
      Res      : DB.String_Vectors.Vector;

      L        : Positive := 1; -- line and column position to check
      C        : Positive := 1;

      procedure Check (Position : in DB.String_Vectors.Cursor);
      --  Print an item

      -----------
      -- Print --
      -----------

      procedure Check (Position : in DB.String_Vectors.Cursor) is
         Value : constant String := DB.String_Vectors.Element (Position);
      begin
         Assert
           (Expected (L)(C) = +Value,
            L'Img & C'Img & " | " &
            "Expected " & To_String (Expected (L)(C)) & " found " & Value);
         C := C + 1;
      end Check;

   begin
      DB.SQLite.Connect (H, "../data/testing.db");

      DB.SQLite.Execute
        (H, "insert into user ('login', 'password', 'email', 'admin')" &
         " values ('toto', 'pwd', 'toto@here.com', 'false')");

      DB.SQLite.Prepare_Select
        (H, I, "select login, password, email from user order by login");

      while DB.SQLite.More (I) loop
         DB.SQLite.Get_Line (I, Res);
         DB.String_Vectors.Iterate (Res, Check'Access);
         DB.String_Vectors.Clear (Res);
         L := L + 1;
         C := 1;
      end loop;

      DB.SQLite.End_Select (I);

      DB.SQLite.Close (H);
   end Simple_User;

end DB_Tests.User;
