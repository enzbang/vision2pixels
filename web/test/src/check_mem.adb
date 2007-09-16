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

with Ada.Command_Line;
with AUnit;

with Gwiad.Web;
with Gwiad.Dynamic_Libraries.Manager;
with V2P.Web_Server;

with Web_Suite;

procedure Check_Mem is
   use Ada;

   use Gwiad;
   use Gwiad.Dynamic_Libraries.Manager;

   procedure Run is
     new AUnit.Test_Runner (Suite => Web_Suite.Web_Suite_Access);

begin
   Web.Start;

   Manager.Discover_Libraries;

   Main_Loop : declare
      use Ada.Command_Line;
      Iteration : constant Positive := Positive'Value (Argument (1));
   begin
      for K in 1 .. Iteration loop
         --  Run tests
         Run;
      end loop;
   end Main_Loop;

   --  Exit now

   Web.Stop;
end Check_Mem;
