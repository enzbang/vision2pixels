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

package body Web_Tests.Last_Visit_After_Post is

   use Ada;

   procedure Check_New (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check !NEW information

   procedure Check_New_Message (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check !NEW information for text message

   procedure Post_Comment (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Post a new comment

   procedure Check_New_After_Comment
     (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check !NEW information after a new comment

   ---------------
   -- Check_New --
   ---------------

   procedure Check_New (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
      Conn   : Client.HTTP_Connection;
   begin
      Client.Create (Conn, "http://" & Host & ':' & Utils.Image (Port));

      delay 1.2;

      Call (Conn, Result, URI => "/");
      Login (Conn, "enzbang", "password");

      --  Check that forum is marked as containing new posts

      Call (Conn, Result, URI => "/");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(+"Forum photographies", +"timestampnew", +"Forum mat"),
             "Forum photograhie should be marked as containing new post");
      end;

      --  Check that the new post is marked as new

      Call (Conn, Result, URI => "/forum/threads?FID=1");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(+"!NEW", +"Un_Troll", +"!NEW", +"Hissez"),
             "New post Un_Troll should be marked as new");
      end;

      --  Check that the comment inside are marked as new too

      Call (Conn, Result, URI => "/forum/entry?TID=142");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(+"!NEW", +"turbo", +"mon_commentaire"),
             "The new comment should be marked as new");
      end;

      --  Check that the new post is now not marked as new

      Call (Conn, Result, URI => "/forum/threads?FID=1");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(not "!NEW", +"Un_Troll", +"/141-", +"Hissez"),
             "New post Un_Troll should now not be marked as new");
      end;

      --  Check that the comment inside is not marked as new now

      Call (Conn, Result, URI => "/forum/entry?TID=142");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(not "!NEW", +"turbo", +"mon_commentaire"),
             "The new comment should not be marked as new");
      end;

      Logout (Conn);
   end Check_New;

   -----------------------
   -- Check_New_Message --
   -----------------------

   procedure Check_New_Message (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
      Conn   : Client.HTTP_Connection;
   begin
      Client.Create (Conn, "http://" & Host & ':' & Utils.Image (Port));

      Call (Conn, Result, URI => "/");
      Login (Conn, "turbo", "turbopass");

      --  Check that turbo's own new post is not marked as new

      Call (Conn, Result, URI => "/forum/threads?FID=2");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(not "!NEW", +"une vente", +"enzbang"),
            "New text post une vente should not be marked as new for turbo");
      end;

      Logout (Conn);

      Call (Conn, Result, URI => "/");
      Login (Conn, "enzbang", "password");

      --  Check that the turbo own new post is not marked as new

      Call (Conn, Result, URI => "/");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(+"Forum photographies", +"Forum mat", +"timestampnew"),
            "Forum should be marked as new for enzbang");
      end;

      Call (Conn, Result, URI => "/forum/threads?FID=2");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(+"turbo", +"!NEW", +"une vente",
              +"Vous", +"vends canon 350D"),
            "New text post une vente should be marked as new for enzbang");
      end;
   end Check_New_Message;

   -----------------------------
   -- Check_New_After_Comment --
   -----------------------------

   procedure Check_New_After_Comment
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Result : Response.Data;
      Conn   : Client.HTTP_Connection;
   begin
      Client.Create (Conn, "http://" & Host & ':' & Utils.Image (Port));

      Call (Conn, Result, URI => "/");
      Login (Conn, "enzbang", "password");

      --  Check that forum is not marked as containing new posts anymore

      Call (Conn, Result, URI => "/");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(not "timestampnew", +"Forum photographies"),
             "Forum photograhie should not be marked as containing new post");
      end;

      --  Check that the new post is marked as new

      Call (Conn, Result, URI => "/forum/threads?FID=1");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(+"!NEW", +"Un_Troll", +"!NEW", +"Hissez"),
             "New post Un_Troll after comment should be marked as new");
      end;

      --  Check that the comment inside are marked as new too

      delay 1.0;

      Call (Conn, Result, URI => "/forum/entry?TID=142");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(not "!NEW", +"turbo", +"mon_commentaire"),
            "Already seen message should not be marked as new");
         Check
           (Page,
            Word_Set'(+"!NEW", +"turbo", +"troll_bof"),
            "The new comment after post should be marked as new");
      end;

      --  Check that the new post is now not marked as new

      Call (Conn, Result, URI => "/forum/threads?FID=1");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
            (Page,
             Word_Set'(not "!NEW", +"Un_Troll"),
             "Already seen post should not be marked as new");
         Check
            (Page,
             Word_Set'(+"!NEW", +"Hissez"),
             "Post not yet seen should still be marked as new");
      end;

      --  Check that the comment inside is not marked as new now

      Call (Conn, Result, URI => "/forum/entry?TID=142");

      declare
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(not "!NEW", +"turbo", +"mon_commentaire",
              +"turbo", +"troll_bof"),
            "The new comment after post should not be marked as new");
      end;

      Logout (Conn);
   end Check_New_After_Comment;

   ------------------
   -- Post_Comment --
   ------------------

   procedure Post_Comment (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;
      Result : Response.Data;
      Conn   : Client.HTTP_Connection;
   begin
      Client.Create (Conn, "http://" & Host & ':' & Utils.Image (Port));

      --  Add a new comment in Un_Troll post

      Call (Conn, Result, URI => "/");
      Login (Conn, "turbo", "turbopass");

      delay 1.2;

      Call
        (Conn, Result,
         URI => Block_New_Comment.Ajax.onsubmit_bnc_comment_register & '?'
           & "forum_photo=t&TID=142&global_comment_input=troll_bof"
           & "&bnc_comment_type=txt&CHECK=V%C3%A9rifier&pfe_PARENT_ID="
         & "&bnc_comment_pid=&REGISTER_COMMENT=Envoyer");

      Logout (Conn);
   end Post_Comment;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.Last_Visit_After_Post");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Check_New'Access, "check new after post");
      Register_Routine
        (T, Check_New_Message'Access, "check new message after post");
      Register_Routine
        (T, Post_Comment'Access, "post a new comment");
      Register_Routine
        (T, Check_New_After_Comment'Access, "check new after comment");
   end Register_Tests;

end Web_Tests.Last_Visit_After_Post;
