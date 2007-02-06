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

with Ada.Text_IO;

with AUnit.Test_Cases.Registration;
with AUnit.Assertions;

with V2P.Wiki;

package body Web_Tests.Wiki is

   use Ada;
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;

   procedure Wiki_To_HTML (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Checks html rendering of wiki comments

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return String_Access is
   begin
      return new String'("Web_Tests.Wiki");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Wiki_To_Html'Access, "wiki to html");
   end Register_Tests;

   ------------------
   -- Wiki_To_HTML --
   ------------------

   procedure Wiki_To_HTML (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert
        (V2P.Wiki.Wiki_To_HTML ("http://simple.url") =
           "<a href='http://simple.url' rel='nofollow'>http://simple.url</a>",
         "Error with http://simple.url");

      Assert
        (V2P.Wiki.Wiki_To_HTML ("some text http://simple.url") =
         ("some text <a href='http://simple.url' rel='nofollow'>"
            & "http://simple.url</a>"),
         "Error with some text http://simple.url");

      Assert
        (V2P.Wiki.Wiki_To_HTML ("http://simple.url some text") =
         ("<a href='http://simple.url' rel='nofollow'>"
            & "http://simple.url</a> some text"),
         "Error with http://simple.url some text");

      Assert
        (V2P.Wiki.Wiki_To_HTML
           ("some text http://simple.url?param=url&param2=url2 some text") =
           ("some text <a href='http://simple.url?param=url&amp;"
            & "param2=url2' rel='nofollow'>"
            & "http://simple.url?param=url&amp;param2=url2</a> some text"),
         V2P.Wiki.Wiki_To_HTML
           ("some text http://simple.url?param=url&param2=url2 some text"));
         --           "Error with some text
         --  http://simple.url.param=url&param2=url2"
         --           & " some text");

      Assert
        (V2P.Wiki.Wiki_To_HTML
           ("some text for [[http://simple.url?param=val&param2=val2]"
            & "[this is a simple url]] well formatted") =
           ("some text for <a href='http://simple.url?param=val&amp;"
            & "param2=val2' "
            & "rel='nofollow'>this is a simple url</a> well formatted"),
         "Error with some text for [[http://simple.url?param=val&param2=val2]"
         &  "[this is a simple url]] well formatted");

      Assert
        (V2P.Wiki.Wiki_To_HTML
           ("[[http://simple.url?param=val&param2=val2]"
            & "[this is a simple url]]") =
           ("<a href='http://simple.url?param=val&amp;param2=val2' "
            & "rel='nofollow'>this is a simple url</a>"),
         "Error with [[http://simple.url?param=val&param2=val2]"
         &  "[this is a simple url]]");

      Assert
        (V2P.Wiki.Wiki_To_HTML
           ("some text for [[http://simple.url?param=val&param2=val2]"
            & "[this is a simple url malformatted") = "some text for ",
         "Error with some text for [[http://simple.url?param=val&param2=val2]"
         &  "[this is a simple url malformatted");

      Assert
        (V2P.Wiki.Wiki_To_HTML ("[em Emphasized text]") =
           "<em>Emphasized text</em>",
         "Error with [em Emphasized text]");

      Assert
        (V2P.Wiki.Wiki_To_HTML ("some text [blockquote some quote]") =
           "some text <blockquote>some quote</blockquote>",
         "Error with some text [blockquote some quote]");

      Assert
        (V2P.Wiki.Wiki_To_HTML ("[strong strong text]") =
           "<strong>strong text</strong>",
         "Error with [strong strong text]");
   end Wiki_To_Html;

end Web_Tests.Wiki;
