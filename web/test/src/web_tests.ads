------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                             --
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

package Web_Tests is

   use Ada.Strings.Unbounded;

   Host : constant String := "localhost";
   --  v2p web server host

   Port : constant := 8080;
   --  v2p web server port

   type Word_Set is array (Positive range <>) of Unbounded_String;

   procedure Check (Page : in String; Word : in Word_Set; Message : in String);
   --  Does nothing if the set of Word appears (in the right order) in Page.
   --  Otherwise it raises an AUnit assertion and log the web page.

   function Get (Page, Regpat : in String; Index : in Positive) return String;
   --  Returns the Index-th match for regpat in Page or the null string if not
   --  found.

   function "+"
     (Str : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   function "not" (Word : in String) return Unbounded_String;
   -- A word that should not be found into the results, this is intended to be
   -- used to build a Word_Set.

end Web_Tests;
