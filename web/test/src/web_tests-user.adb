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

with Ada.Text_IO;

with AUnit.Test_Cases.Registration;
with AUnit.Assertions;

with AWS.Client;
with AWS.Response;
with AWS.Utils;

with V2P.Template_Defs.Block_Login;

package body Web_Tests.User is

   use Ada;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use AWS;
   use V2P.Template_Defs;

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  The very first thing to do is to get the main page

   procedure Login (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Log as administrateur into the application

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

   -----------
   -- Login --
   -----------

   procedure Login (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;

      function Login_Parameters (Login, Password : in String) return String;
      --  Returns the HTTP login parameters

      ----------------------
      -- Login_Parameters --
      ----------------------

      function Login_Parameters (Login, Password : in String) return String is
      begin
         return Block_Login.HTTP.Login & '=' & Login &
           '&' & Block_Login.HTTP.Password & '=' & Password;
      end Login_Parameters;

   begin
      Client.Get
        (Connection, Result,
         URI => Block_Login.Ajax.Onclick_Login_Form_Enter &
         '?' & Login_Parameters ("turbo", "password"));

      Check
        (Response.Message_Body (Result),
         (+"apply_style", +"status_bar", +"display", +"block",
          +"apply_style", +"forum_post", +"display", +"none",
          +"apply_style", +"new_comment", +"display", +"none"),
         "login should have failed for turbo");

      Client.Get
        (Connection, Result,
         URI => Block_Login.Ajax.Onclick_Login_Form_Enter &
         '?' & Login_Parameters ("turbo", "turbopass"));

      Check
        (Response.Message_Body (Result),
         (+"apply_style", +"status_bar", +"display", +"none",
          +"apply_style", +"forum_post", +"display", +"block",
          +"replace", +"login",
          +"replace", +"comment_login",
          +"apply_style", +"new_comment", +"display", +"block"),
         "login failed for turbo");
   end Login;

   ---------------
   -- Main_Page --
   ---------------

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));

      Client.Get (Connection, Result, URI => "/");

      Check
        (Response.Message_Body (Result),
         (+"Forum photographies", +"Forum mat"),
         "cannot get the first page");
   end Main_Page;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return String_Access is
   begin
      return new String'("Web_Tests.User");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Main_Page'Access, "main page");
      Register_Routine (T, Login'Access, "user login");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

end Web_Tests.User;
