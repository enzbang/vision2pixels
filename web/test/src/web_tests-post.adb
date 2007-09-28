------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2007                             --
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

with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Block_Forum_Filter;

package body Web_Tests.Post is

   use AWS;

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  The very first thing to do is to get the main page

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Close the Web connection

   procedure Check_Post (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  List threads in a forum

   Connection : Client.HTTP_Connection;
   --  Server connection used by all tests

   Context    : Unbounded_String;
   --  The context Id to be passed with each request

   -----------
   -- Close --
   -----------

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Client.Close (Connection);
   end Close;

   ----------------
   -- Check_Post --
   ----------------

   procedure Check_Post
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use V2P.Template_Defs;

      Result : Response.Data;

      function URL_Context return String;
      --  Returns the context as an HTTP URL parameter

      -----------------
      -- URL_Context --
      -----------------

      function URL_Context return String is
      begin
         return "CTX_WB=" & To_String (Context);
      end URL_Context;

   begin
      Client.Get
        (Connection, Result, URI => "/forum/entry?TID=89&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"Invasion", +"Portrait",
           +"non révélé", +"commentaire pour invasion",
           +"L'auteur", +"Alors qu'en pensez-vous?",
           +"enzbang", +"Bof!",
           +"L'auteur", +"Mais encore ?"),
         "wrong entry for post 89");
   end Check_Post;

   ---------------
   -- Main_Page --
   ---------------

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class) is
      R_Context : constant String := "div id=""CTX_WB""[^>]+>([^<]+)";
      Result    : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));

      Client.Get (Connection, Result, URI => "/");

      Check_Page : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(1 => +"Forum photographies", 2 => +"Forum mat"),
            "cannot get the first page");

         Context := +Get (Page, R_Context, 1);

         Assert (Context /= Null_Unbounded_String, "No context found!");
      end Check_Page;
   end Main_Page;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Message_String is
   begin
      return New_String ("Web_Tests.Post");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Main_Page'Access, "main page");
      Register_Routine (T, Check_Post'Access, "check post");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

end Web_Tests.Post;
