------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2012                             --
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

with AUnit.Assertions;

with AWS.Client;
with AWS.Response;
with AWS.Utils;

with V2P.Template_Defs.Block_Theme_Admin;
with V2P.Template_Defs.Block_Theme_Photos;

package body Web_Tests.Themes is

   use AWS;
   use V2P.Template_Defs;

   procedure Themes (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check the main page

   procedure Vote_Stage1 (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check vote stage 1

   procedure Vote_Stage2 (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check vote stage 2

   procedure Vote_Final (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check vote final

   procedure Check_Theme_Result (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Check the past theme page

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

   ------------------------
   -- Check_Theme_Result --
   ------------------------

   procedure Check_Theme_Result
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Result : Response.Data;
   begin
      Logout (Connection);

      Call (Connection, Result, URI => "/theme/1");

      Check_0 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"En N&B",
               2  => +"Le train en marche",
               3  => +"Vainqueur",
               4  => +"Désséché",
               5  => +"Finaliste",
               6  => +"Stylo"),
            "wrong content for thème: En N&B");
      end Check_0;

      Call (Connection, Result, URI => "/theme/2");

      Check_1 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Tour E",
               3  => +"Vainqueur",
               4  => +"Quel gland  !",
               5  => +"Finaliste",
               6  => +"By night",
               7  => +"Rides"),
            "wrong content for thème: Tout en bleu");
      end Check_1;
   end Check_Theme_Result;

   ------------
   -- Themes --
   ------------

   procedure Themes (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 1",
               3  => +"4 photographies",
               4  => not "restant",
               5  => not "Etape suivante",
               6  => +"By night",
               7  => +"Tour E",
               8  => +"Quel gland  !",
               9  => +"Rides",
               10 => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page (anonymous)");
      end Check_Page;

      --  Logged as admin

      Login (Connection, "turbo", "turbopass");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page_Turbo : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 1",
               3  => +"4 photographies",
               4  => not "restant",
               5  => +"Etape suivante",
               6  => +"By night",
               7  => +"Tour E",
               8  => +"Quel gland  !",
               9  => +"Rides",
               10 => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page (turbo)");
      end Check_Page_Turbo;

      --  Logged as standard user

      Logout (Connection);
      Login (Connection, "test", "test");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page_Test : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 1",
               3  => +"4 photographies",
               4  => not "restant",
               5  => not "Etape suivante",
               6  => +"By night",
               7  => +"Tour E",
               8  => +"Quel gland  !",
               9  => +"Rides",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page (test)");
      end Check_Page_Test;
   end Themes;

   ----------------
   -- Vote_Final --
   ----------------

   procedure Vote_Final (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Response.Data;
   begin
      Login (Connection, "turbo", "turbopass");

      Call
        (Connection, Result,
         URI => Block_Theme_Admin.Ajax.onclick_bta_next_stage);
      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page_Stage1 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1 => +"Thèmes précédents",
               2 => +"Tout en bleu",
               3 => not "Quel gland  !",
               4 => +"Tour E",
               5 => +"N&B",
               6 => +"Le train en marche"),
            "wrong entries in the Themes page (final stage)");
      end Check_Page_Stage1;
   end Vote_Final;

   -----------------
   -- Vote_Stage1 --
   -----------------

   procedure Vote_Stage1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Response.Data;
   begin
      Logout (Connection);
      Login (Connection, "turbo", "turbopass");

      Call (Connection, Result, URI => "/module/Thèmes");

      --  Move to next stage

      Call
        (Connection, Result,
         URI => Block_Theme_Admin.Ajax.onclick_bta_next_stage);
      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page_Stage1 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 2",
               3  => +"4 photographies",
               4  => +"3 votes restant",
               5  => +"Etape suivante",
               6  => +"Tour E",
               7  => +"Quel gland  !",
               8  => +"Rides",
               9  => +"By night",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page (stage 1)");
      end Check_Page_Stage1;

      --  Do some vote (turbo)

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "3?PHOTO_ID=67");
      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "4?PHOTO_ID=87");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Vote_1 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 2",
               3  => +"4 photographies",
               4  => +"1 vote restant",
               5  => +"Etape suivante",
               6  => +"Tour E",
               7  => +"Quel gland  !",
               8  => +"Rides",
               9  => +"By night",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page after vote (stage 1)");
      end Check_Vote_1;

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "2?PHOTO_ID=39");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Vote_2 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 2",
               3  => +"4 photographies",
               4  => +"0 vote restant",
               5  => +"Etape suivante",
               6  => +"Tour E",
               7  => +"Quel gland  !",
               8  => +"Rides",
               9  => +"By night",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page after vote (stage 1)");
      end Check_Vote_2;

      --  Do some vote (test)

      Logout (Connection);
      Login (Connection, "test", "test");

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "2?PHOTO_ID=39");

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "3?PHOTO_ID=67");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Vote_3 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 2",
               3  => +"4 photographies",
               4  => +"1 vote restant",
               5  => not "Etape suivante",
               6  => +"By night",
               7  => +"Tour E",
               8  => +"Quel gland  !",
               9  => +"Rides",
               10 => +"btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page after vote test (stage 1)");
      end Check_Vote_3;

      --  Back to turbo, check the count of votes

      Logout (Connection);
      Login (Connection, "turbo", "turbopass");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Vote_Count : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1 => +"(39 - 2)",
               2 => +"(67 - 2)",
               3 => +"(87 - 1)",
               4 => +"Etape suivante"),
            "wrong count of vote");
      end Check_Vote_Count;
   end Vote_Stage1;

   -----------------
   -- Vote_Stage2 --
   -----------------

   procedure Vote_Stage2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Response.Data;
   begin
      Logout (Connection);
      Login (Connection, "turbo", "turbopass");

      Call (Connection, Result, URI => "/module/Thèmes");

      --  Move to next stage

      Call
        (Connection, Result,
         URI => Block_Theme_Admin.Ajax.onclick_bta_next_stage);
      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Page_Stage2 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 3",
               3  => +"4 photographies",
               4  => +"1 vote restant",
               5  => +"Etape suivante",
               6  => +"Tour E",
               7  => +"Quel gland  !",
               8  => +"By night",
               9  => +"Rides",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page (stage 2)");
      end Check_Page_Stage2;

      --  Do a vote

      Logout (Connection);
      Login (Connection, "test", "test");

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "3?PHOTO_ID=67");

      Logout (Connection);
      Login (Connection, "turbo", "turbopass");

      Call
        (Connection, Result,
         URI => Block_Theme_Photos.Ajax.onclick_btp_sel & "2?PHOTO_ID=39");

      Call (Connection, Result, URI => "/module/Thèmes");

      Check_Vote_1 : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'
              (1  => +"Tout en bleu",
               2  => +"Etape 3",
               3  => +"4 photographies",
               4  => +"0 vote restant",
               5  => +"Etape suivante",
               6  => +"Tour E",
               7  => +"Quel gland  !",
               8  => +"By night",
               9  => +"Rides",
               10  => not "btp_sel",
               11 => +"Thèmes précédents",
               12 => +"N&B",
               13 => +"Le train en marche"),
            "wrong entries in the Themes page after vote (stage 1)");
      end Check_Vote_1;
   end Vote_Stage2;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format ("Web_Tests.Themes");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Themes'Access, "themes page");
      Register_Routine (T, Vote_Stage1'Access, "check stage 1");
      Register_Routine (T, Vote_Stage2'Access, "check stage 2");
      Register_Routine (T, Vote_Final'Access, "check vote final");
      Register_Routine (T, Check_Theme_Result'Access, "check themes result");
   end Register_Tests;

end Web_Tests.Themes;
