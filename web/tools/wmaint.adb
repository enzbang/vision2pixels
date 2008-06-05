------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2008                             --
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

with AWS.Config;
with AWS.MIME;
with AWS.Server;
with AWS.Status;
with AWS.Response;

procedure Wmain is

   use AWS;

   function Callback (S : in Status.Data) return Response.Data;

   S : Server.HTTP;
   C : Config.Object := Config.Get_Current;

   --------------
   -- Callback --
   --------------

   function Callback (S : in Status.Data) return Response.Data is
   begin
      return Response.Build
        (MIME.Text_HTML,
         "<p>Serveur en maintenance... veuillez patienter.</p>");
   end Callback;

begin
   Server.Start (S, Callback'Unrestricted_Access, C);
   Server.Wait (Server.Forever);
end Wmain;
