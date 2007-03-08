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

with Ada.Strings.Unbounded;
with GNAT.Regpat;

package body V2P.Wiki is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;

   function Extract_Links (S : in String) return String;
   --  Extract all http:// links

   function Web_Escape (S : in String) return String;
   --  Escape HTML special characters

   function Wiki_Format (S : in String) return String;
   --  Transforms Wiki syntax into XHTML

   -------------------
   -- Extract_Links --
   -------------------

   function Extract_Links (S : in String) return String is
      Link_Extract : constant Pattern_Matcher :=
                       Compile ("(http://([^ \s\[\]]+))", Case_Insensitive);
      --  Gets all http:// links that do not contain white space
      --  or '[' and ']' characters
      Matches      : Match_Array (0 .. 6);
      Current      : Natural := S'First;
      Result       : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Link_Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         --  Search if it is a formatted link
         --  [[http://link.to.website][website name]]

         if Matches (1).First > 2
           and then Matches (1).Last < S'Last - 4
           and then S (Matches (1).First - 2 .. Matches (1).First - 1) = "[["
           and then S (Matches (1).Last + 1 .. Matches (1).Last + 2) = "]["
         then
            --  Search for ']]'
            for K in Matches (1).Last + 2 .. S'Last - 1 loop
               if S (K .. K + 1) = "]]" then
                  Result := Result & S (Current .. Matches (1).First - 3)
                    & "<a href='" & S (Matches (1).First .. Matches (1).Last)
                    & "' rel='nofollow'>"
                    & S (Matches (1).Last + 3 .. K - 1)
                    & "</a>";
                  Current := K + 2;
                  exit;
               end if;

               if K = S'Last - 1 then
                  --  End of String and link malformatted. Skip it.
                  Result := Result & S (Current .. Matches (1).First - 3);
                  return To_String (Result);
               end if;
            end loop;
         end if;

         if Current <= Matches (1).First then
            --  Non formatted url http://...

            Result := Result & S (Current .. Matches (1).First - 1)
              & "<a href='" & S (Matches (1).First .. Matches (1).Last)
              & "' rel='nofollow'>"
              & S (Matches (1).First .. Matches (1).Last)
              & "</a>";

            Current := Matches (1).Last + 1;
         end if;
      end loop;

      Result := Result & S (Current .. S'Last);
      return To_String (Result);
   end Extract_Links;

   ----------------
   -- Web_Escape --
   ----------------

   function Web_Escape (S : in String) return String is

      Result : Unbounded_String;
      Last   : Integer := S'First;

      procedure Append_To_Result
        (Str : in String; From : in Integer; To : in Integer);
      --  Append S (From .. To) to Result if not empty concatenated with Str
      --  and update Last.

      ----------------------
      -- Append_To_Result --
      ----------------------

      procedure Append_To_Result
        (Str : in String; From : in Integer; To : in Integer) is
      begin
         if From <= To then
            Append (Result, S (From .. To) & Str);
         else
            Append (Result, Str);
         end if;

         Last := To + 2;
      end Append_To_Result;

   begin
      for I in S'Range loop
         case S (I) is
            when '&'    => Append_To_Result ("&amp;", Last, I - 1);
            when '>'    => Append_To_Result ("&gt;", Last, I - 1);
            when '<'    => Append_To_Result ("&lt;", Last, I - 1);
            when '"'    => Append_To_Result ("&quot;", Last, I - 1);
            when others => null;
         end case;
      end loop;

      if Last <= S'Last then
         Append (Result, S (Last .. S'Last));
      end if;

      return To_String (Result);
   end Web_Escape;

   -----------------
   -- Wiki_Format --
   -----------------

   function Wiki_Format (S : in String) return String is
      Extract : constant Pattern_Matcher :=
                  Compile ("\[(\w+) (.+?)\]", Case_Insensitive);
      --  Gets all [keyword string]
      --  where keyword is em, blockquote or strong

      Matches : Match_Array (0 .. 2);
      Current : Natural := S'First;
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         Result := Result & S (Current .. Matches (0).First - 1);

         declare
            Keyword : constant String :=
                        S (Matches (1).First .. Matches (1).Last);
         begin
            if Keyword = "em" then
               Result := Result & "<em>"
                 & S (Matches (2).First .. Matches (2).Last) & "</em>";
            elsif Keyword = "blockquote" then
               Result := Result & "<blockquote>"
                 & S (Matches (2).First .. Matches (2).Last) & "</blockquote>";
            elsif Keyword = "strong" then
               Result := Result & "<strong>"
                 & S (Matches (2).First .. Matches (2).Last) & "</strong>";
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      Result := Result & S (Current .. S'Last);
      return To_String (Result);
   end Wiki_Format;

   ------------------
   -- Wiki_To_HTML --
   ------------------

   function Wiki_To_HTML (S : in String) return String is
      Without_Html : constant String := Web_Escape (S);
      With_Links   : constant String := Extract_Links (Without_Html);
   begin
      return Wiki_Format (With_Links);
   end Wiki_To_HTML;

end V2P.Wiki;
