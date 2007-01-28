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

with GNAT.Regpat;
with Ada.Strings.Unbounded;

package body V2P.Wiki is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;

   function Strip_HTML_Tag (S : in String) return String;
   --  Strip all HTML tags <*>

   function Extract_Links (S : in String) return String;
   --  Extract all http:// links

   function Extract_Links (S : in String) return String is
      Link_Extract : constant Pattern_Matcher
        := Compile ("(http://([^ \s\[\]]+))",
                    Case_Insensitive);
      --  Gets all http:// links that do not contain white space
      --  or '[' and ']' characters
      Matches      : Match_Array (0 .. 6);
      Current      : Natural := S'First;
      Result       : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         Match (Link_Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         --  Search if it is a formatted link
         --  [[http://link.to.website][website name]]

         if Matches (1).First > 2 and then Matches (1).Last < S'Last - 4
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

   function Strip_HTML_Tag (S : in String) return String is
      Result : String (S'Range);
      K      : Positive := Result'First;
      J      : Positive := S'First;
   begin
      if S = "" then
         return "";
      end if;

      loop
         if S (J) = '<' then
            while S (J) /= '>' and J /= S'Last loop
               J := J + 1;
            end loop;
            J := J + 1;
         else
            Result (K) := S (J);
            K := K + 1;
            J := J + 1;
         end if;
         exit when J > S'Last;
      end loop;

      return Result (Result'First .. K - 1);
   end Strip_HTML_Tag;

   function Wiki_To_Html (S : in String) return String is
      Without_Html : constant String := Strip_HTML_Tag (S);
      With_Links   : constant String := Extract_Links (Without_Html);
   begin
      return With_Links;
   end Wiki_To_Html;

end V2P.Wiki;
