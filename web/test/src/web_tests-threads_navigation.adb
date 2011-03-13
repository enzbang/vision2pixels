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
with V2P.Template_Defs.Block_Forum_Sort;

package body Web_Tests.Threads_Navigation is

   use AWS;

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  The very first thing to do is to get the main page

   procedure Forum_Photo (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Go to the photography forum page

   procedure Close (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Close the Web connection

   procedure List_Forum_Threads (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  List threads in a forum

   procedure Set_Page_Size (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Set page size to 100

   procedure Set_Last_Posted (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Set filter to last posted

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

   -----------------
   -- Forum_Photo --
   -----------------

   procedure Forum_Photo (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Call (Connection, Result, URI => "/forum/threads?FID=1");
   end Forum_Photo;

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

      Call (Connection, Result, URI => "/forum/threads?FID=1");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"/141-", +"Hissez haut...", +"Paysage",
           +"/89-", +"Invasion", +"Portrait",
           +"/88-", +"On ne pousse pas", +"Paysage",
           +"/87-", +"Désséché", +"Abstrait",
           +"/140-", +"Rides", +"Nature morte",
           +"/139-", +"Coucher de soleil", +"Paysage",
           +"/138-", +"Ah Parisssse", +"Paysage",
           +"/137-", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"/136-", +"Keys", +"Abstrait",
           +"/135-", +"Côtes", +"Paysage",
           +"/134-", +"Yoda", +"Portrait",
           +"/133-", +"Arc en ciel", +"Paysage",
           +"/132-", +"En troupeau", +"Macro/Animaux",
           +"/131-", +"Road", +"Paysage",
           +"/130-", +"Bridge", +"Paysage",
           +"/129-", +"This way", +"Abstrait",
           +"/128-", +"Spip", +"Macro/Animaux",
           +"/127-", +"Vert", +"Nature morte",
           +"/126-", +"Smoking... No smoking", +"Macro/Animaux",
           +"/125-", +"En chemin", +"Paysage",
           +"/124-", +"Parapentiste", +"Portrait",
           +"/123-", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"/122-", +"Ecologie ?", +"Abstrait",
           +"/121-", +"Cascades", +"Paysage",
           +"/120-", +"Quel gland  !", +"Nature morte",
           +"/119-", +"On the road, zooming again", +"Abstrait",
           +"/118-", +"London", +"Paysage",
           +"/117-", +"Gourmandises", +"Abstrait",
           +"/116-", +"GDF", +"Abstrait",
           +"/115-", +"Touches", +"Abstrait",
           +"/114-", +"H2O", +"Abstrait",
           +"/113-", +"Pas à pas", +"Paysage",
           +"/112-", +"Quel ciel !", +"Paysage",
           +"/111-", +"Espagne", +"Paysage",
           +"/110-", +"Neige", +"Paysage",
           +"/109-", +"Balle", +"Macro/Animaux",
           +"/108-", +"Motif", +"Abstrait",
           +"/107-", +"L'Europe", +"Paysage",
           +"/106-", +"A quai", +"Paysage",
           +"/105-", +"Le train en marche", +"Paysage",
           +"/104-", +"Without you I'm nothing", +"Macro/Animaux",
           +"/103-", +"Rouages", +"Macro/Animaux",
           +"/102-", +"Campagne", +"Paysage",
           +"/101-", +"Batracien", +"Macro/Animaux",
           +"/100-", +"Abstract", +"Abstrait",
           +"/99-", +"United color of", +"Macro/Animaux",
           +"/98-", +"Feuille", +"Macro/Animaux",
           +"/97-", +"Home sweet home", +"Abstrait",
           +"/96-", +"Changement de direction", +"Abstrait",
           +"/95-", +"Saturne", +"Paysage",
           +"/94-", +"Cabane ", +"Paysage",
           +"/93-", +"Mine de rien", +"Macro/Animaux",
           +"/92-", +"Tour E", +"Paysage",
           +"/91-", +"Bus", +"Paysage",
           +"/90-", +"Haut en couleurs", +"Portrait",
           +"/86-", +"Notre guide", +"Paysage",
           +"/85-", +"Port", +"Paysage",
           +"/84-", +"Perdu ?", +"Macro/Animaux",
           +"/63-", +"Fleurs", +"Macro/Animaux",
           +"/62-", +"Fire", +"Nature morte",
           +"/61-", +"Stylo", +"Nature morte",
           +"/60-", +"Ribik", +"Nature morte",
           +"/59-", +"Blé", +"Paysage",
           +"/58-", +"By night", +"Paysage",
           +"/57-", +"Ponton", +"Paysage",
           +"/56-", +"Manger une fraise", +"Portrait",
           +"/55-", +"Cheminées", +"Paysage",
           +"/54-", +"un camion", +"Paysage",
           +"/67-", +"Un soleil ?", +"Abstrait",
           +"/66-", +"La Grande Roue", +"Abstrait",
           +"/65-", +"Kiwi", +"Nature morte",
           +"/64-", +"Chaises", +"Nature morte",
           +"/75-", +"Parlons ensemble", +"Nature morte",
           +"/74-", +"Violon", +"Nature morte",
           +"/73-", +"Manque d'eau", +"Paysage",
           +"/72-", +"Lock", +"Nature morte",
           +"/71-", +"Figée", +"Nature morte",
           +"/70-", +"Livre", +"Nature morte",
           +"/69-", +"4", +"Abstrait",
           +"/68-", +"Eglise", +"Paysage",
           +"/83-", +"Belles dents", +"Macro/Animaux",
           +"/82-", +"Même pas dans mes cauchemars", +"Macro/Animaux",
           +"/81-", +"Liens", +"Nature morte",
           +"/80-", +"Ca passe par là", +"Abstrait",
           +"/79-", +"Entre deux eaux", +"Macro/Animaux",
           +"/78-", +"Envie de sport", +"Paysage",
           +"/77-", +"Oeuf", +"Nature morte",
           +"/76-", +"Couché de soleil", +"Paysage"),
         "wrong set of images in thread FID=1 (all posts)");

      --  Today posts

      Call
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TODAY");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"/141-", +"Hissez haut...", +"Paysage",
           +"/89-", +"Invasion", +"Portrait",
           +"/88-", +"On ne pousse pas", +"Paysage",
           +"/87-", +"Désséché", +"Abstrait",
           not "/140-", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (today posts)");

      --  Two days

      Call
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TWO_DAYS");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"/141-", +"Hissez haut...", +"Paysage",
           +"/89-", +"Invasion", +"Portrait",
           +"/88-", +"On ne pousse pas", +"Paysage",
           +"/87-", +"Désséché", +"Abstrait",
           +"/140-", +"Rides", +"Nature morte",
           +"/139-", +"Coucher de soleil", +"Paysage",
           +"/138-", +"Ah Parisssse", +"Paysage",
           +"/137-", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"/136-", +"Keys", +"Abstrait",
           +"/135-", +"Côtes", +"Paysage",
           +"/134-", +"Yoda", +"Portrait",
           +"/133-", +"Arc en ciel", +"Paysage",
           +"/132-", +"En troupeau", +"Macro/Animaux",
           +"/131-", +"Road", +"Paysage",
           +"/130-", +"Bridge", +"Paysage",
           +"/129-", +"This way", +"Abstrait",
           +"/128-", +"Spip", +"Macro/Animaux",
           +"/127-", +"Vert", +"Nature morte",
           +"/126-", +"Smoking... No smoking", +"Macro/Animaux",
           +"/125-", +"En chemin", +"Paysage",
           +"/124-", +"Parapentiste", +"Portrait",
           +"/123-", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"/122-", +"Ecologie ?", +"Abstrait",
           +"/121-", +"Cascades", +"Paysage",
           +"/120-", +"Quel gland  !", +"Nature morte",
           +"/119-", +"On the road, zooming again", +"Abstrait",
           +"/118-", +"London", +"Paysage",
           +"/117-", +"Gourmandises", +"Abstrait",
           +"/116-", +"GDF", +"Abstrait",
           +"/115-", +"Touches", +"Abstrait",
           +"/114-", +"H2O", +"Abstrait",
           +"/113-", +"Pas à pas", +"Paysage",
           +"/112-", +"Quel ciel !", +"Paysage",
           +"/111-", +"Espagne", +"Paysage",
           +"/110-", +"Neige", +"Paysage",
           +"/109-", +"Balle", +"Macro/Animaux",
           +"/108-", +"Motif", +"Abstrait",
           +"/107-", +"L'Europe", +"Paysage",
           +"/106-", +"A quai", +"Paysage",
           +"/105-", +"Le train en marche", +"Paysage",
           +"/104-", +"Without you I'm nothing", +"Macro/Animaux",
           +"/103-", +"Rouages", +"Macro/Animaux",
           +"/102-", +"Campagne", +"Paysage",
           +"/101-", +"Batracien", +"Macro/Animaux",
           +"/100-", +"Abstract", +"Abstrait",
           +"/99-", +"United color of", +"Macro/Animaux",
           +"/98-", +"Feuille", +"Macro/Animaux",
           +"/97-", +"Home sweet home", +"Abstrait",
           +"/96-", +"Changement de direction", +"Abstrait",
           +"/95-", +"Saturne", +"Paysage",
           +"/94-", +"Cabane ", +"Paysage",
           +"/93-", +"Mine de rien", +"Macro/Animaux",
           +"/92-", +"Tour E", +"Paysage",
           +"/91-", +"Bus", +"Paysage",
           +"/90-", +"Haut en couleurs", +"Portrait",
           +"/86-", +"Notre guide", +"Paysage",
           +"/85-", +"Port", +"Paysage",
           +"/84-", +"Perdu ?", +"Macro/Animaux",
           not "/63-", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (tow days posts)");

      --  Seven days

      Call
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=SEVEN_DAYS");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"<response>",
           +"/141-", +"Hissez haut...", +"Paysage",
           +"/89-", +"Invasion", +"Portrait",
           +"/88-", +"On ne pousse pas", +"Paysage",
           +"/87-", +"Désséché", +"Abstrait",
           +"/140-", +"Rides", +"Nature morte",
           +"/139-", +"Coucher de soleil", +"Paysage",
           +"/138-", +"Ah Parisssse", +"Paysage",
           +"/137-", +"Un éléphant ça trompe...", +"Macro/Animaux",
           +"/136-", +"Keys", +"Abstrait",
           +"/135-", +"Côtes", +"Paysage",
           +"/134-", +"Yoda", +"Portrait",
           +"/133-", +"Arc en ciel", +"Paysage",
           +"/132-", +"En troupeau", +"Macro/Animaux",
           +"/131-", +"Road", +"Paysage",
           +"/130-", +"Bridge", +"Paysage",
           +"/129-", +"This way", +"Abstrait",
           +"/128-", +"Spip", +"Macro/Animaux",
           +"/127-", +"Vert", +"Nature morte",
           +"/126-", +"Smoking... No smoking", +"Macro/Animaux",
           +"/125-", +"En chemin", +"Paysage",
           +"/124-", +"Parapentiste", +"Portrait",
           +"/123-", +"While My Guitar Gently Weeps", +"Macro/Animaux",
           +"/122-", +"Ecologie ?", +"Abstrait",
           +"/121-", +"Cascades", +"Paysage",
           +"/120-", +"Quel gland  !", +"Nature morte",
           +"/119-", +"On the road, zooming again", +"Abstrait",
           +"/118-", +"London", +"Paysage",
           +"/117-", +"Gourmandises", +"Abstrait",
           +"/116-", +"GDF", +"Abstrait",
           +"/115-", +"Touches", +"Abstrait",
           +"/114-", +"H2O", +"Abstrait",
           +"/113-", +"Pas à pas", +"Paysage",
           +"/112-", +"Quel ciel !", +"Paysage",
           +"/111-", +"Espagne", +"Paysage",
           +"/110-", +"Neige", +"Paysage",
           +"/109-", +"Balle", +"Macro/Animaux",
           +"/108-", +"Motif", +"Abstrait",
           +"/107-", +"L'Europe", +"Paysage",
           +"/106-", +"A quai", +"Paysage",
           +"/105-", +"Le train en marche", +"Paysage",
           +"/104-", +"Without you I'm nothing", +"Macro/Animaux",
           +"/103-", +"Rouages", +"Macro/Animaux",
           +"/102-", +"Campagne", +"Paysage",
           +"/101-", +"Batracien", +"Macro/Animaux",
           +"/100-", +"Abstract", +"Abstrait",
           +"/99-", +"United color of", +"Macro/Animaux",
           +"/98-", +"Feuille", +"Macro/Animaux",
           +"/97-", +"Home sweet home", +"Abstrait",
           +"/96-", +"Changement de direction", +"Abstrait",
           +"/95-", +"Saturne", +"Paysage",
           +"/94-", +"Cabane ", +"Paysage",
           +"/93-", +"Mine de rien", +"Macro/Animaux",
           +"/92-", +"Tour E", +"Paysage",
           +"/91-", +"Bus", +"Paysage",
           +"/90-", +"Haut en couleurs", +"Portrait",
           +"/86-", +"Notre guide", +"Paysage",
           +"/85-", +"Port", +"Paysage",
           +"/84-", +"Perdu ?", +"Macro/Animaux",
           +"/63-", +"Fleurs", +"Macro/Animaux",
           +"/62-", +"Fire", +"Nature morte",
           +"/61-", +"Stylo", +"Nature morte",
           +"/60-", +"Ribik", +"Nature morte",
           +"/59-", +"Blé", +"Paysage",
           +"/58-", +"By night", +"Paysage",
           +"/57-", +"Ponton", +"Paysage",
           +"/56-", +"Manger une fraise", +"Portrait",
           +"/55-", +"Cheminées", +"Paysage",
           +"/54-", +"un camion", +"Paysage",
           +"/67-", +"Un soleil ?", +"Abstrait",
           +"/66-", +"La Grande Roue", +"Abstrait",
           +"/65-", +"Kiwi", +"Nature morte",
           +"/64-", +"Chaises", +"Nature morte",
           not "/75-", +"</ul>", +"</response>"),
         "wrong set of images in thread FID=1 (seven days posts)");

      --  Check that the filter is kept into the context

      Call
        (Connection,
         Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=TODAY");

      Call (Connection, Result, URI => "/forum/threads?FID=1");

      Check
        (Response.Message_Body (Result),
         Word_Set'(+"/141-", +"Hissez haut...", +"Paysage",
           +"/89-", +"Invasion", +"Portrait",
           +"/88-", +"On ne pousse pas", +"Paysage",
           +"/87-", +"Désséché", +"Abstrait",
           not "/140-"),
         "Filter context not properly restored");
   end List_Forum_Threads;

   ---------------
   -- Main_Page --
   ---------------

   procedure Main_Page (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Response.Data;
   begin
      Client.Create (Connection, "http://" & Host & ':' & Utils.Image (Port));

      Call (Connection, Result, URI => "/");

      Check_Page : declare
         use AUnit.Assertions;
         Page : constant String := Response.Message_Body (Result);
      begin
         Check
           (Page,
            Word_Set'(1 => +"Forum photographies", 2 => +"Forum mat"),
            "cannot get the first page");
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
      Register_Routine (T, Forum_Photo'Access, "go to photography forum");
      Register_Routine (T, Set_Page_Size'Access, "Set page size");
      Register_Routine (T, Set_All_Messages'Access, "Set all messages");
      Register_Routine (T, Set_Last_Posted'Access, "Set last posted");
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
      Call
        (Connection, Result,
         URI => Block_Forum_Filter.Ajax.onchange_bff_forum_filter_set
         & "?" & Block_Forum_Filter.HTTP.bff_forum_filter_set
         & "=ALL_MESSAGES");
   end Set_All_Messages;

   ---------------------
   -- Set_Last_Posted --
   ---------------------

   procedure Set_Last_Posted (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;

      Result : Response.Data;
   begin
      Call
        (Connection, Result,
         URI => Block_Forum_Sort.Ajax.onchange_bfs_forum_sort_set
         & "?" & Block_Forum_Sort.HTTP.bfs_forum_sort_set
         & "=LAST_POSTED");
   end Set_Last_Posted;

   -------------------
   -- Set_Page_Size --
   -------------------

   procedure Set_Page_Size (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use V2P.Template_Defs;

      Result : Response.Data;
   begin
      Call
        (Connection, Result,
         URI => Block_Forum_Filter_Page_Size.Ajax.
           onchange_bffps_forum_filter_pagesize
         & "?" & Block_Forum_Filter_Page_Size.HTTP.bffps_forum_filter_pagesize
         & "=100");
   end Set_Page_Size;

end Web_Tests.Threads_Navigation;
