------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2008-2009                          --
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

with AWS.Client;
with AWS.Response;
with AWS.Utils;

package body Web_Tests.Forum_Entry is

   procedure Connect (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Connect to server

   procedure Check_Exif (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check exif information

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Close the Web connection

   Connection : Client.HTTP_Connection;
   --  Server connection used by all tests

   ----------------
   -- Check_Exif --
   ----------------

   procedure Check_Exif (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Get
        (Connection, Result, URI => "/forum/entry?TID=67&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"ISO", +"Date de création", +"2.8", +"Multi-segment"),
         "should have exif data");

      Client.Get
        (Connection, Result, URI => "/forum/entry?TID=89&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(not "Marque", not "Model"),
         "should not have exif information as not revealed");
   end Check_Exif;

   -----------
   -- Close --
   -----------

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Client.Close (Connection);
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));
      Client.Get (Connection, Result, URI => "/");
      Set_Context (Response.Message_Body (Result));
   end Connect;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.Forum_Entry");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Connect'Access, "connect to server");
      Register_Routine (T, Check_Exif'Access, "exif on forum entry");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (T : in out Test_Case) is
   begin
      Set_Context;
   end Set_Up_Case;

end Web_Tests.Forum_Entry;
