------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2010                            --
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

with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Response;
with AWS.Utils;

with V2P.Template_Defs.Block_Login;
with V2P.Template_Defs.Block_New_Comment;

package body Web_Tests.Last_Visit is

   use Ada;

   procedure Check_New (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check exif information

   ---------------
   -- Check_New --
   ---------------

   procedure Check_New (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;
      Result : Response.Data;
      Conn1 : Client.HTTP_Connection;
      Conn2 : Client.HTTP_Connection;
   begin
      Client.Create (Conn1, "http://" & Host & ':' & Utils.Image (Port));
      Client.Create (Conn2, "http://" & Host & ':' & Utils.Image (Port));

      Call (Conn1, Result, URI => "/");
      Login (Conn1, "enzbang", "password");

      Call (Conn1, Result, URI => "/forum/threads?FID=1");

      --  Now read Hissez haut...
      Call (Conn1, Result, URI => "/forum/entry?TID=141");

      --  For the timestamp to differ
      delay 2.0;

      Call (Conn1, Result, URI => "/forum/threads?FID=1");

      declare
         Page  : constant String := Response.Message_Body (Result);
         Index : constant Natural := Strings.Fixed.Index (Page, "TID=71");
      begin
         --  Cut the Page before the second post.
         Check
            (Page (Page'First .. Index + 5),
         Word_Set'(not "!NEW", +"Hissez"),
         "Should not have !NEW before 'Hissez haut...'");
      end;

      Logout (Conn1);

      --  Add a new comment in Hissez haut... post
      Call (Conn2, Result, URI => "/");
      Login (Conn2, "turbo", "turbopass");

      Call
        (Conn2, Result,
         URI => Block_New_Comment.Ajax.onsubmit_bnc_comment_register & '?'
           & "forum_photo=t&TID=141&global_comment_input=nouveau"
           & "&bnc_comment_type=txt&CHECK=V%C3%A9rifier&pfe_PARENT_ID="
         & "&bnc_comment_pid=&REGISTER_COMMENT=Envoyer");

      --  turbo just post a comment, this post should not be marked as !NEW
      Call (Conn2, Result, URI => "/forum/threads?FID=1");

      declare
         Page  : constant String := Response.Message_Body (Result);
         Index : constant Natural := Strings.Fixed.Index (Page, "TID=71");
      begin
         --  Cut the Page before the second post.
         Check
            (Page (Page'First .. Index + 5),
         Word_Set'(not "!NEW", +"Hissez"),
         "Should not have !NEW before 'Hissez haut...' for turbo");
      end;

      Logout (Conn2);
      Client.Close (Conn2);

      Login (Conn1, "enzbang", "password");
      Call (Conn1, Result, URI => "/forum/threads?FID=1");

      declare
         Page  : constant String := Response.Message_Body (Result);
         Index : constant Natural := Strings.Fixed.Index (Page, "TID=71");
      begin
         --  Cut the Page before the second post.
         Check
            (Page (Page'First .. Index + 5),
         Word_Set'(+"!NEW", +"Hissez"),
         "Should have !NEW before 'Hissez haut...'");
      end;

      Client.Close (Conn1);
   end Check_New;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.Last_Visit");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_New'Access, "check new");
   end Register_Tests;

end Web_Tests.Last_Visit;
