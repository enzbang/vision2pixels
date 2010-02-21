------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2009                          --
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

with AUnit;
with AUnit.Assertions;

with AWS.Response;
with AWS.Utils;

with V2P.Template_Defs.Block_Login;

package body Web_Tests is

   use Ada;

   R_Context : constant String := "div id=""CTX_WB""[^>]+>([^<]+)";
   A_Context : constant String := "ctx id=""([^""]+)""";

   Context : Unbounded_String;
   --  The context Id to be passed with each request

   function "-"
     (Str : in Unbounded_String)
      return String
      renames To_String;

   Log_File : Text_IO.File_Type;

   type Coding is record
      From, To : Unbounded_String;
   end record;

   procedure Set_Context (Page : in String := "");
   --  Set context from page content, if Page is empty string the context is
   --  deleted.

   procedure Set_Ajax_Context (XML : in String);
   --  Set context from Ajax XML content

   function Add_Context (URI : in String) return String;
   --  Add current context to the given URI

   function URL_Context return String;
   --  Returns the context as an HTTP URL parameter

   type Coding_Array is array (1 .. 30) of Coding;

   Coding_Rules : Coding_Array;

   -----------
   -- "not" --
   -----------

   function "not" (Word : in String) return Unbounded_String is
   begin
      return +("(not)" & Word);
   end "not";

   -----------------
   -- Add_Context --
   -----------------

   function Add_Context (URI : in String) return String is
   begin
      if Strings.Fixed.Index (URI, "?") = 0 then
         return URI & '?' & URL_Context;
      else
         return URI & '&' & URL_Context;
      end if;
   end Add_Context;

   ----------
   -- Call --
   ----------

   procedure Call
     (Connection : in out AWS.Client.HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String)
   is
   begin
      Client.Get (Connection, Result, Add_Context (URI));

      if Strings.Fixed.Index (URI, "$") = 0 then
         Set_Context (Response.Message_Body (Result));
      else
         --  An Ajax call
         Set_Ajax_Context (Response.Message_Body (Result));
      end if;
   end Call;

   -----------
   -- Check --
   -----------

   procedure Check
     (Page    : in String;
      Word    : in Word_Set;
      Message : in String)
   is
      use AUnit;
      use AUnit.Assertions;

      function Get (Word : in Word_Set; K : in Positive) return String;
      --  Returns Word (k) (skip the not operator if present)

      function Is_Not (Word : in String) return Boolean;
      --  Returns True if Word contains the not operator

      procedure Log (Str : in String; Message : in String);
      --  Add Str into the log with the message decription as header

      ---------
      -- Get --
      ---------

      function Get (Word : in Word_Set; K : in Positive) return String is
         W : constant String := -Word (K);
      begin
         if W'Length >= 5 and then W (W'First .. W'First + 4) = "(not)" then
            return W (W'First + 5 .. W'Last);
         end if;
         return W;
      end Get;

      ------------
      -- Is_Not --
      ------------

      function Is_Not (Word : in String) return Boolean is
         Result : Boolean := False;
      begin
         if Word'Length >= 5
           and then Word (Word'First .. Word'First + 4) = "(not)"
         then
            Result := True;
         end if;
         return Result;
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

      E_Page : Unbounded_String := +Page;
      Status : Boolean := True;
      P      : Natural := 1;
      Len, I : Natural;

   begin
      --  First apply all the encoding rules to the Web page

      Handle_Rules : for K in Coding_Rules'Range loop
         Rule : declare
            From : constant String := -Coding_Rules (K).From;
         begin
            Apply_Rule : loop
               I := Index (E_Page, From);

               exit Apply_Rule when I = 0;

               Replace_Slice
                 (E_Page, I, I + From'Length - 1, -Coding_Rules (K).To);
            end loop Apply_Rule;
         end Rule;
      end loop Handle_Rules;

      --  Check the page now

      Len := Length (E_Page);

      Check_Status_Page : for K in Word'Range loop
         Check_Word : declare
            use Ada.Strings.Fixed;
            W   : constant String := Get (Word, K);
            Tmp : Natural;
         begin
            Tmp := Index (Slice (E_Page, P, Len), W);

            if Is_Not (-Word (K)) then
               if Tmp /= 0 then
                  I := K;
                  Status := False;
                  exit Check_Status_Page;
               end if;

            else
               if Tmp = 0 then
                  I := K;
                  Status := False;
                  exit Check_Status_Page;
               else
                  P := Tmp + W'Length;
               end if;
            end if;
         end Check_Word;

         exit Check_Status_Page when P > Len;
      end loop Check_Status_Page;

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

   function Get
     (Page, Regpat : in String; Index : in Positive) return String
   is
      use GNAT.Regpat;
      R_Context : constant Pattern_Matcher := Compile (Regpat);
      Matches   : Match_Array (Match_Count range 0 .. 10);
   begin
      Match (R_Context, Page, Matches);

      if Matches (Index) = No_Match then
         return "";
      end if;
      return Page (Matches (Index).First .. Matches (Index).Last);
   end Get;

   -----------
   -- Login --
   -----------

   procedure Login
     (Connection : in out Client.HTTP_Connection; User, Password : in String)
   is
      use V2P.Template_Defs;

      Result : Response.Data;

   begin
      Call
        (Connection, Result,
         URI => Block_Login.Ajax.onclick_bl_login_form_enter &
         '?' & Login_Parameters (User, Password));
   end Login;

   ----------------------
   -- Login_Parameters --
   ----------------------

   function Login_Parameters (Login, Password : in String) return String is
      use V2P.Template_Defs;
   begin
      return Block_Login.HTTP.bl_login_input & '=' & Login &
        '&' & Block_Login.HTTP.bl_password_input & '=' & Password &
        '&' & Block_Login.HTTP.bl_remember_me & "=0";
   end Login_Parameters;

   ------------
   -- Logout --
   ------------

   procedure Logout (Connection : in out Client.HTTP_Connection) is
      use V2P.Template_Defs;
      Result : Response.Data;
   begin
      Call
        (Connection, Result, URI => Block_Login.Ajax.onclick_bl_logout_enter);
   end Logout;

   ----------------------
   -- Set_Ajax_Context --
   ----------------------

   procedure Set_Ajax_Context (XML : in String) is
      use AUnit.Assertions;
   begin
      Context := +Get (XML, A_Context, 1);
      Assert (URL_Context /= Null_Unbounded_String,
              "No context found in Ajax response!");
   end Set_Ajax_Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context (Page : in String := "") is
      use AUnit.Assertions;
   begin
      if Page = "" then
         Context := Null_Unbounded_String;
      else
         Context := +Get (Page, R_Context, 1);
         Assert (URL_Context /= Null_Unbounded_String,
                 "No context found in Web page!");
      end if;
   end Set_Context;

   ------------
   -- Upload --
   ------------

   procedure Upload
     (Connection : in out AWS.Client.HTTP_Connection;
      Result     :    out Response.Data;
      Filename   : in     String;
      URI        : in     String)
   is
   begin
      Client.Upload
        (Connection, Result, Filename => Filename, URI => Add_Context (URI));
      Set_Context (Response.Message_Body (Result));
   end Upload;

   -----------------
   -- URL_Context --
   -----------------

   function URL_Context return String is
   begin
      return "CTX_WB=" & To_String (Context);
   end URL_Context;

begin --  Web_Tests : Initialization
   Coding_Rules :=
     Coding_Array'(Coding'(+"&eacute;", +"é"),
      Coding'(+"&egrave;", +"è"),
      Coding'(+"&ecirc;", +"ê"),
      Coding'(+"&euml;", +"ë"),

      Coding'(+"&aacute;", +"&#225;"),
      Coding'(+"&agrave;", +"à"),
      Coding'(+"&acirc;", +"â"),
      Coding'(+"&atilde;", +"&#227;"),
      Coding'(+"&auml;", +"ä"),

      Coding'(+"&iacute;", +"&#237;"),
      Coding'(+"&igrave;", +"&#236;"),
      Coding'(+"&icirc;", +"î"),
      Coding'(+"&iuml;", +"ï"),

      Coding'(+"&oacute;", +"&#243;"),
      Coding'(+"&ograve;", +"&#242;"),
      Coding'(+"&ocirc;", +"ô"),
      Coding'(+"&otilde;", +"&#245;"),
      Coding'(+"&ouml;", +"ö"),

      Coding'(+"&uacute;", +"&#250;"),
      Coding'(+"&ugrave;", +"ù"),
      Coding'(+"&ucirc;", +"û"),
      Coding'(+"&uuml;", +"ü"),

      Coding'(+"&ccedil;", +"ç"),
      Coding'(+"&ntilde;", +"&#241;"),
      Coding'(+"&aelig;", +"&#230;"),
      Coding'(+"&lt;", +"<"),
      Coding'(+"&gt;", +">"),
      Coding'(+"&amp;", +"&"),
      Coding'(+"&apos;", +"'"),
      Coding'(+"&quot;", +""""));

   Text_IO.Create
     (Log_File, Mode => Text_IO.Out_File, Name => "web_tests.log");
end Web_Tests;
