------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2009                            --
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

package body Web_Tests.RSS is

   procedure Last_Posts (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Checks last post content

   procedure Last_Comments (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Checks last comments

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Close the Web connection

   Connection : Client.HTTP_Connection;
   --  Server connection used by all tests

   -----------
   -- Close --
   -----------

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Client.Close (Connection);
   end Close;

   -------------------
   -- Last_Comments --
   -------------------

   procedure Last_Comments (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Get (Connection, Result, URI => "/rss/comments");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"L'auteur", +"Mais encore", +"turbo", +"Superbe",
           +"turbo", +"com en imafe", +"test", +"prefere l'original",
           +"turbo", +"une autre proposition", +"enzbang", +"Bof!",
           +"enzbang", +"Un classique", +"enzbang", +"^_^"),
         "wrong content for RSS last comments:"
         & Response.Message_Body (Result));
   end Last_Comments;

   ----------------
   -- Last_Posts --
   ----------------

   procedure Last_Posts (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));
      Client.Get (Connection, Result, URI => "/rss/posts");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"Un_Troll", +"photographies", +"Portrait",
           +"Hissez haut...", +"photographies", +"Paysage",
           +"Invasion", +"photographies", +"Portrait",
           +"On ne pousse pas", +"photographies", +"Paysage",
           +"Rides", +"En troupeau", +"Road"),
         "wrong content for RSS last post:"
         & Response.Message_Body (Result));
   end Last_Posts;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.RSS");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Last_Posts'Access, "RSS: last posts");
      Register_Routine (T, Last_Comments'Access, "RSS: last comments");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

end Web_Tests.RSS;
