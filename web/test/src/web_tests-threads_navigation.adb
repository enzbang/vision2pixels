------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_Forum_Filter_Page_Size;

package body Web_Tests.Threads_Navigation is

   use AWS;

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  The very first thing to do is to get the main page

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Close the Web connection

   procedure List_Forum_Threads (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  List threads in a forum

   procedure Set_Page_Size (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Set page size to 500

   procedure Set_All_Messages (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Set filter to all messages which is expected to be the initial setting
   --  for the test.

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
   -- List_Forum_Threads --
   ------------------------

   procedure List_Forum_Threads
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use V2P.Template_Defs;

      Result : Response.Data;
   begin
      --  All posts

      Client.Get
        (Connection, Result, URI => "/forum/threads?FID=1&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"TID=141", +"Hissez haut...", +"Paysage",
           +"TID=89", +"Invasion", +"Portrait",
           +"TID=88", +"On ne pousse pas", +"Paysage",
           +"TID=87", +"Désséché", +"Abstrait",
           +"TID=140", +"Rides", +"Nature morte",
           +"TID=139", +"Coucher de soleil", +"Paysage",
           +"TID=138", +"Ah Parisssse", +"Paysage",
           +"TID=137", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"TID=136", +"Keys", +"Abstrait",
           +"TID=135", +"Côtes", +"Paysage",
           +"TID=134", +"Yoda", +"Portrait",
           +"TID=133", +"Arc en ciel", +"Paysage",
           +"TID=132", +"En troupeau", +"Macro/Animaux",
           +"TID=131", +"Road", +"Paysage",
           +"TID=130", +"Bridge", +"Paysage",
           +"TID=129", +"This way", +"Abstrait",
           +"TID=128", +"Spip", +"Macro/Animaux",
           +"TID=127", +"Vert", +"Nature morte",
           +"TID=126", +"Smoking... No smoking", +"Macro/Animaux",
           +"TID=125", +"En chemin", +"Paysage",
           +"TID=124", +"Parapentiste", +"Portrait",
           +"TID=123", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"TID=122", +"Ecologie ?", +"Abstrait",
           +"TID=121", +"Cascades", +"Paysage",
           +"TID=120", +"Quel gland  !", +"Nature morte",
           +"TID=119", +"On the road, zooming again", +"Abstrait",
           +"TID=118", +"London", +"Paysage",
           +"TID=117", +"Gourmandises", +"Abstrait",
           +"TID=116", +"GDF", +"Abstrait",
           +"TID=115", +"Touches", +"Abstrait",
           +"TID=114", +"H2O", +"Abstrait",
           +"TID=113", +"Pas à pas", +"Paysage",
           +"TID=112", +"Quel ciel !", +"Paysage",
           +"TID=111", +"Espagne", +"Paysage",
           +"TID=110", +"Neige", +"Paysage",
           +"TID=109", +"Balle", +"Macro/Animaux",
           +"TID=108", +"Motif", +"Abstrait",
           +"TID=107", +"L'Europe", +"Paysage",
           +"TID=106", +"A quai", +"Paysage",
           +"TID=105", +"Le train en marche", +"Paysage",
           +"TID=104", +"Without you I'm nothing", +"Macro/Animaux",
           +"TID=103", +"Rouages", +"Macro/Animaux",
           +"TID=102", +"Campagne", +"Paysage",
           +"TID=101", +"Batracien", +"Macro/Animaux",
           +"TID=100", +"Abstract", +"Abstrait",
           +"TID=99", +"United color of", +"Macro/Animaux",
           +"TID=98", +"Feuille", +"Macro/Animaux",
           +"TID=97", +"Home sweet home", +"Abstrait",
           +"TID=96", +"Changement de direction", +"Abstrait",
           +"TID=95", +"Saturne", +"Paysage",
           +"TID=94", +"Cabane ", +"Paysage",
           +"TID=93", +"Mine de rien", +"Macro/Animaux",
           +"TID=92", +"Tour E", +"Paysage",
           +"TID=91", +"Bus", +"Paysage",
           +"TID=90", +"Haut en couleurs", +"Portrait",
           +"TID=86", +"Notre guide", +"Paysage",
           +"TID=85", +"Port", +"Paysage",
           +"TID=84", +"Perdu ?", +"Macro/Animaux",
           +"TID=63", +"Fleurs", +"Macro/Animaux",
           +"TID=62", +"Fire", +"Nature morte",
           +"TID=61", +"Stylo", +"Nature morte",
           +"TID=60", +"Ribik", +"Nature morte",
           +"TID=59", +"Blé", +"Paysage",
           +"TID=58", +"By night", +"Paysage",
           +"TID=57", +"Ponton", +"Paysage",
           +"TID=56", +"Manger une fraise", +"Portrait",
           +"TID=55", +"Cheminées", +"Paysage",
           +"TID=54", +"un camion", +"Paysage",
           +"TID=67", +"Un soleil ?", +"Abstrait",
           +"TID=66", +"La Grande Roue", +"Abstrait",
           +"TID=65", +"Kiwi", +"Nature morte",
           +"TID=64", +"Chaises", +"Nature morte",
           +"TID=75", +"Parlons ensemble", +"Nature morte",
           +"TID=74", +"Violon", +"Nature morte",
           +"TID=73", +"Manque d'eau", +"Paysage",
           +"TID=72", +"Lock", +"Nature morte",
           +"TID=71", +"Figée", +"Nature morte",
           +"TID=70", +"Livre", +"Nature morte",
           +"TID=69", +"4", +"Abstrait",
           +"TID=68", +"Eglise", +"Paysage",
           +"TID=83", +"Belles dents", +"Macro/Animaux",
           +"TID=82", +"Même pas dans mes cauchemars", +"Macro/Animaux",
           +"TID=81", +"Liens", +"Nature morte",
           +"TID=80", +"Ca passe par là", +"Abstrait",
           +"TID=79", +"Entre deux eaux", +"Macro/Animaux",
           +"TID=78", +"Envie de sport", +"Paysage",
           +"TID=77", +"Oeuf", +"Nature morte",
           +"TID=76", +"Couché de soleil", +"Paysage"),
         "wrong set of images in thread FID=1 (all posts)");

      --  Today posts

      Client.Get
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TODAY&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"TID=141", +"Hissez haut...", +"Paysage",
           +"TID=89", +"Invasion", +"Portrait",
           +"TID=88", +"On ne pousse pas", +"Paysage",
           +"TID=87", +"Désséché", +"Abstrait",
           not "TID=140", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (today posts)");

      --  Two days

      Client.Get
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TWO_DAYS&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"TID=141", +"Hissez haut...", +"Paysage",
           +"TID=89", +"Invasion", +"Portrait",
           +"TID=88", +"On ne pousse pas", +"Paysage",
           +"TID=87", +"Désséché", +"Abstrait",
           +"TID=140", +"Rides", +"Nature morte",
           +"TID=139", +"Coucher de soleil", +"Paysage",
           +"TID=138", +"Ah Parisssse", +"Paysage",
           +"TID=137", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"TID=136", +"Keys", +"Abstrait",
           +"TID=135", +"Côtes", +"Paysage",
           +"TID=134", +"Yoda", +"Portrait",
           +"TID=133", +"Arc en ciel", +"Paysage",
           +"TID=132", +"En troupeau", +"Macro/Animaux",
           +"TID=131", +"Road", +"Paysage",
           +"TID=130", +"Bridge", +"Paysage",
           +"TID=129", +"This way", +"Abstrait",
           +"TID=128", +"Spip", +"Macro/Animaux",
           +"TID=127", +"Vert", +"Nature morte",
           +"TID=126", +"Smoking... No smoking", +"Macro/Animaux",
           +"TID=125", +"En chemin", +"Paysage",
           +"TID=124", +"Parapentiste", +"Portrait",
           +"TID=123", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"TID=122", +"Ecologie ?", +"Abstrait",
           +"TID=121", +"Cascades", +"Paysage",
           +"TID=120", +"Quel gland  !", +"Nature morte",
           +"TID=119", +"On the road, zooming again", +"Abstrait",
           +"TID=118", +"London", +"Paysage",
           +"TID=117", +"Gourmandises", +"Abstrait",
           +"TID=116", +"GDF", +"Abstrait",
           +"TID=115", +"Touches", +"Abstrait",
           +"TID=114", +"H2O", +"Abstrait",
           +"TID=113", +"Pas à pas", +"Paysage",
           +"TID=112", +"Quel ciel !", +"Paysage",
           +"TID=111", +"Espagne", +"Paysage",
           +"TID=110", +"Neige", +"Paysage",
           +"TID=109", +"Balle", +"Macro/Animaux",
           +"TID=108", +"Motif", +"Abstrait",
           +"TID=107", +"L'Europe", +"Paysage",
           +"TID=106", +"A quai", +"Paysage",
           +"TID=105", +"Le train en marche", +"Paysage",
           +"TID=104", +"Without you I'm nothing", +"Macro/Animaux",
           +"TID=103", +"Rouages", +"Macro/Animaux",
           +"TID=102", +"Campagne", +"Paysage",
           +"TID=101", +"Batracien", +"Macro/Animaux",
           +"TID=100", +"Abstract", +"Abstrait",
           +"TID=99", +"United color of", +"Macro/Animaux",
           +"TID=98", +"Feuille", +"Macro/Animaux",
           +"TID=97", +"Home sweet home", +"Abstrait",
           +"TID=96", +"Changement de direction", +"Abstrait",
           +"TID=95", +"Saturne", +"Paysage",
           +"TID=94", +"Cabane ", +"Paysage",
           +"TID=93", +"Mine de rien", +"Macro/Animaux",
           +"TID=92", +"Tour E", +"Paysage",
           +"TID=91", +"Bus", +"Paysage",
           +"TID=90", +"Haut en couleurs", +"Portrait",
           +"TID=86", +"Notre guide", +"Paysage",
           +"TID=85", +"Port", +"Paysage",
           +"TID=84", +"Perdu ?", +"Macro/Animaux",
           not "TID=63", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (tow days posts)");

      --  Seven days

      Client.Get
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=SEVEN_DAYS&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"TID=141", +"Hissez haut...", +"Paysage",
           +"TID=89", +"Invasion", +"Portrait",
           +"TID=88", +"On ne pousse pas", +"Paysage",
           +"TID=87", +"Désséché", +"Abstrait",
           +"TID=140", +"Rides", +"Nature morte",
           +"TID=139", +"Coucher de soleil", +"Paysage",
           +"TID=138", +"Ah Parisssse", +"Paysage",
           +"TID=137", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"TID=136", +"Keys", +"Abstrait",
           +"TID=135", +"Côtes", +"Paysage",
           +"TID=134", +"Yoda", +"Portrait",
           +"TID=133", +"Arc en ciel", +"Paysage",
           +"TID=132", +"En troupeau", +"Macro/Animaux",
           +"TID=131", +"Road", +"Paysage",
           +"TID=130", +"Bridge", +"Paysage",
           +"TID=129", +"This way", +"Abstrait",
           +"TID=128", +"Spip", +"Macro/Animaux",
           +"TID=127", +"Vert", +"Nature morte",
           +"TID=126", +"Smoking... No smoking", +"Macro/Animaux",
           +"TID=125", +"En chemin", +"Paysage",
           +"TID=124", +"Parapentiste", +"Portrait",
           +"TID=123", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"TID=122", +"Ecologie ?", +"Abstrait",
           +"TID=121", +"Cascades", +"Paysage",
           +"TID=120", +"Quel gland  !", +"Nature morte",
           +"TID=119", +"On the road, zooming again", +"Abstrait",
           +"TID=118", +"London", +"Paysage",
           +"TID=117", +"Gourmandises", +"Abstrait",
           +"TID=116", +"GDF", +"Abstrait",
           +"TID=115", +"Touches", +"Abstrait",
           +"TID=114", +"H2O", +"Abstrait",
           +"TID=113", +"Pas à pas", +"Paysage",
           +"TID=112", +"Quel ciel !", +"Paysage",
           +"TID=111", +"Espagne", +"Paysage",
           +"TID=110", +"Neige", +"Paysage",
           +"TID=109", +"Balle", +"Macro/Animaux",
           +"TID=108", +"Motif", +"Abstrait",
           +"TID=107", +"L'Europe", +"Paysage",
           +"TID=106", +"A quai", +"Paysage",
           +"TID=105", +"Le train en marche", +"Paysage",
           +"TID=104", +"Without you I'm nothing", +"Macro/Animaux",
           +"TID=103", +"Rouages", +"Macro/Animaux",
           +"TID=102", +"Campagne", +"Paysage",
           +"TID=101", +"Batracien", +"Macro/Animaux",
           +"TID=100", +"Abstract", +"Abstrait",
           +"TID=99", +"United color of", +"Macro/Animaux",
           +"TID=98", +"Feuille", +"Macro/Animaux",
           +"TID=97", +"Home sweet home", +"Abstrait",
           +"TID=96", +"Changement de direction", +"Abstrait",
           +"TID=95", +"Saturne", +"Paysage",
           +"TID=94", +"Cabane ", +"Paysage",
           +"TID=93", +"Mine de rien", +"Macro/Animaux",
           +"TID=92", +"Tour E", +"Paysage",
           +"TID=91", +"Bus", +"Paysage",
           +"TID=90", +"Haut en couleurs", +"Portrait",
           +"TID=86", +"Notre guide", +"Paysage",
           +"TID=85", +"Port", +"Paysage",
           +"TID=84", +"Perdu ?", +"Macro/Animaux",
           +"TID=63", +"Fleurs", +"Macro/Animaux",
           +"TID=62", +"Fire", +"Nature morte",
           +"TID=61", +"Stylo", +"Nature morte",
           +"TID=60", +"Ribik", +"Nature morte",
           +"TID=59", +"Blé", +"Paysage",
           +"TID=58", +"By night", +"Paysage",
           +"TID=57", +"Ponton", +"Paysage",
           +"TID=56", +"Manger une fraise", +"Portrait",
           +"TID=55", +"Cheminées", +"Paysage",
           +"TID=54", +"un camion", +"Paysage",
           +"TID=67", +"Un soleil ?", +"Abstrait",
           +"TID=66", +"La Grande Roue", +"Abstrait",
           +"TID=65", +"Kiwi", +"Nature morte",
           +"TID=64", +"Chaises", +"Nature morte",
           not "TID=75", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (seven days posts)");

      --  Check that the filter is kept into the context

      Client.Get
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TODAY&" & URL_Context);

      Client.Get
        (Connection, Result, URI => "/forum/threads?FID=1&" & URL_Context);

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"TID=141", +"Hissez haut...", +"Paysage",
           +"TID=89", +"Invasion", +"Portrait",
           +"TID=88", +"On ne pousse pas", +"Paysage",
           +"TID=87", +"Désséché", +"Abstrait",
           not "TID=140"),
         "Filter context not properly restored");
   end List_Forum_Threads;

   ---------------
   -- Main_Page --
   ---------------

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
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

         Set_Context (Page);

         Assert (URL_Context /= Null_Unbounded_String, "No context found!");
      end Check_Page;
   end Main_Page;

   ----------
   -- Name --
   ----------

   overriding function Name (T : in Test_Case) return Message_String is
   begin
      return Format ("Web_Tests.Threads_Navigation");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Main_Page'Access, "main page");
      Register_Routine (T, Set_Page_Size'Access, "Set page size");
      Register_Routine (T, Set_All_Messages'Access, "Set all messages");
      Register_Routine (T, List_Forum_Threads'Access, "list post");
      Register_Routine (T, Close'Access, "close connection");
   end Register_Tests;

   ----------------------
   -- Set_All_Messages --
   ----------------------

   procedure Set_All_Messages (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;

      Result : Response.Data;
   begin
      Client.Get
        (Connection, Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=ALL_MESSAGES&" & URL_Context);
   end Set_All_Messages;

   -------------------
   -- Set_Page_Size --
   -------------------

   procedure Set_Page_Size (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;

      Result : Response.Data;
   begin
      Client.Get
        (Connection, Result,
         URI => Block_Forum_Filter_Page_Size.Ajax.
           onchange_bffps_forum_filter_pagesize
         & "?" & Block_Forum_Filter_Page_Size.HTTP.bffps_forum_filter_pagesize
         & "=500&" & URL_Context);
   end Set_Page_Size;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (T : in out Test_Case) is
   begin
      Set_Context;
   end Set_Up_Case;

end Web_Tests.Threads_Navigation;
