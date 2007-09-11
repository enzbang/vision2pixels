------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                        Copyright (C) 2006-2007                           --
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

with AUnit; use AUnit; use AUnit.Test_Suites;

with Web_Tests.Threads_Navigation;
with Web_Tests.User;
with Web_Tests.Wiki;

package body Web_Suite is
   Web_Suite_Test : Access_Test_Suite;

   function Web_Suite_Access return Access_Test_Suite is
   begin
      return Web_Suite_Test;
   end Web_Suite_Access;

begin --  Web_Suite : Initialize the web_suite test

   Web_Suite_Test := new Test_Suite;
   Add_Test (Web_Suite_Test, new Web_Tests.Wiki.Test_Case);
   Add_Test (Web_Suite_Test, new Web_Tests.User.Test_Case);
   Add_Test (Web_Suite_Test, new Web_Tests.Threads_Navigation.Test_Case);
end Web_Suite;

