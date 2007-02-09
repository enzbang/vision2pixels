------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                             --
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
with Ada.Text_IO;

with GNAT.Regpat;

with AUnit.Assertions; use AUnit.Assertions;
with AWS.Utils;

package body Web_Tests is

   use Ada;
   use Ada.Strings.Fixed;

   function "-"
     (Str : in Unbounded_String)
      return String
      renames To_String;

   procedure Log (Str : in String; Message : in String);
   --  Add Str into the log with the message decription as header

   Log_File : Text_IO.File_Type;

   function Is_Not (Word : in String) return Boolean;
   --  Returns True if Word contains the not operator

   function Get (Word : in Word_Set; K : in Positive) return String;
   --  Returns Word (k) (skip the not operator if present)

   type Coding is record
      From, To : Unbounded_String;
   end record;

   Coding_Rules : array (1 .. 30) of Coding;

   -----------
   -- "not" --
   -----------

   function "not" (Word : in String) return Unbounded_String is
   begin
      return +("(not)" & Word);
   end "not";

   -----------
   -- Check --
   -----------

   procedure Check
     (Page    : in String;
      Word    : in Word_Set;
      Message : in String)
   is
      E_Page : Unbounded_String := +Page;
      Status : Boolean := True;
      P, Tmp : Natural;
      Len, I : Natural;
   begin
      --  First apply all the encoding rules to the Web page

      for K in Coding_Rules'Range loop
         declare
            From : constant String := -Coding_Rules (K).From;
         begin
            loop
               I := Index (E_Page, From);

               exit when I = 0;

               Replace_Slice
                 (E_Page, I, I + From'Length - 1, -Coding_Rules (K).To);
            end loop;
         end;
      end loop;

      --  Check the page now

      P := 1;
      Len := Length (E_Page);

      for K in Word'Range loop
         declare
            W : constant String := Get (Word, K);
         begin
            Tmp := Index (Slice (E_Page, P, Len), W);

            if Is_Not (-Word (K)) then
               if Tmp /= 0 then
                  I := K;
                  Status := False;
                  exit;
               end if;

            else
               if Tmp = 0 then
                  I := K;
                  Status := False;
                  exit;
               else
                  P := Tmp + W'Length;
               end if;
            end if;
         end;

         exit when P > Len;
      end loop;

      if not Status then
         Log (To_String (E_Page),
              Message
              & " (word='" & (-Word (I)) & ''' & Natural'Image (I) & ')');
      end if;

      Assert (Status, Message);
   end Check;

   ---------
   -- Get --
   ---------

   function Get (Word : in Word_Set; K : in Positive) return String is
      W : constant String := -Word (K);
   begin
      if W'Length >= 5 and then W (W'First .. W'First + 4) = "(not)" then
         return W (W'First + 5 .. W'Last);
      else
         return W;
      end if;
   end Get;

   function Get
     (Page, Regpat : in String; Index : in Positive) return String
   is
      use GNAT.Regpat;
      R_Context : constant Pattern_Matcher := Compile (Regpat);
      Matches   : Match_Array (0 .. 10);
   begin
      Match (R_Context, Page, Matches);

      if Matches (Index) = No_Match then
         return "";
      else
         return Page (Matches (Index).First .. Matches (Index).Last);
      end if;
   end Get;

   ------------
   -- Is_Not --
   ------------

   function Is_Not (Word : in String) return Boolean is
   begin
      if Word'Length >= 5
        and then Word (Word'First .. Word'First + 4) = "(not)"
      then
         return True;
      else
         return False;
      end if;
   end Is_Not;

   ---------
   -- Log --
   ---------

   procedure Log (Str : in String; Message : in String) is
   begin
      Text_IO.Put_Line (Log_File, "----------------------------------");
      Text_IO.Put_Line (Log_File, ">>> " & Message);
      Text_IO.Put_Line (Log_File, Str);
      Text_IO.New_Line (Log_File);
   end Log;

begin
   Coding_Rules :=
     ((+"&eacute;", +"é"),
      (+"&egrave;", +"è"),
      (+"&ecirc;", +"ê"),
      (+"&euml;", +"ë"),

      (+"&aacute;", +"&#225;"),
      (+"&agrave;", +"à"),
      (+"&acirc;", +"â"),
      (+"&atilde;", +"&#227;"),
      (+"&auml;", +"ä"),

      (+"&iacute;", +"&#237;"),
      (+"&igrave;", +"&#236;"),
      (+"&icirc;", +"î"),
      (+"&iuml;", +"ï"),

      (+"&oacute;", +"&#243;"),
      (+"&ograve;", +"&#242;"),
      (+"&ocirc;", +"ô"),
      (+"&otilde;", +"&#245;"),
      (+"&ouml;", +"ö"),

      (+"&uacute;", +"&#250;"),
      (+"&ugrave;", +"ù"),
      (+"&ucirc;", +"û"),
      (+"&uuml;", +"ü"),

      (+"&ccedil;", +"ç"),
      (+"&ntilde;", +"&#241;"),
      (+"&aelig;", +"&#230;"),
      (+"&lt;", +"<"),
      (+"&gt;", +">"),
      (+"&amp;", +"&"),
      (+"&apos;", +"'"),
      (+"&quot;", +""""));

   Text_IO.Create (Log_File, Text_IO.Out_File, "web_tests.log");
end Web_Tests;
