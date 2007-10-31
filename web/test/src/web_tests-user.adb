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

with Ada.Text_IO;

with AWS.Client;
with AWS.Response;
with AWS.Utils;

with V2P.Template_Defs.Block_Login;

package body Web_Tests.User is

   use AWS;

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
      use V2P.Template_Defs;

      Result : Response.Data;

      function Login_Parameters (Login, Password : in String) return String;
      --  Returns the HTTP login parameters

      ----------------------
      -- Login_Parameters --
      ----------------------

      function Login_Parameters (Login, Password : in String) return String is
      begin
         return Block_Login.HTTP.LOGIN & '=' & Login &
           '&' & Block_Login.HTTP.PASSWORD & '=' & Password;
      end Login_Parameters;

   begin
      Client.Get
        (Connection, Result,
         URI => Block_Login.Ajax.onclick_bl_login_form_enter &
         '?' & Login_Parameters ("turbo", "password"));

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"apply_style", +"login_err", +"display", +"block",
           +"apply_style", +"forum_post", +"display", +"none",
           +"apply_style", +"new_comment", +"display", +"none"),
         "login should have failed for turbo");

      Client.Get
        (Connection, Result,
         URI => Block_Login.Ajax.onclick_bl_login_form_enter &
         '?' & Login_Parameters ("turbo", "turbopass"));

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"apply_style", +"login_err", +"display", +"none",
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
         Word_Set'(+"Forum photographies", +"Forum mat"),
         "cannot get the first page");
   end Main_Page;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Message_Strings.Message_String is
   begin
      return Message_Strings.New_String ("Web_Tests.User");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Main_Page'Access, "main page");
      Register_Routine (T, Login'Access, "user login");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

end Web_Tests.User;
