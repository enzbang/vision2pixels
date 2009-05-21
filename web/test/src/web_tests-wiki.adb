------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with Ada.Text_IO;

with AUnit.Assertions;

with V2P.Wiki;

package body Web_Tests.Wiki is

   procedure Wiki_To_HTML (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Checks html rendering of wiki comments

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.Wiki");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Wiki_To_HTML'Access, "wiki to html");
   end Register_Tests;

   ------------------
   -- Wiki_To_HTML --
   ------------------

   procedure Wiki_To_HTML (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Result : constant String := V2P.Wiki.Wiki_To_HTML ("http://simple.url");
   begin
      Assert
        (Result =
           "<p><a href=" & '"' & "http://simple.url"
               & '"' & ">http://simple.url</a></p>" & ASCII.Lf,
         "Error with http://simple.url, found: '" & Result & ''');
   end Wiki_To_Html;

end Web_Tests.Wiki;
