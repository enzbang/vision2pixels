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

--  Log support for Vision2Pixels
--
--  [TIME] [MODULE_NAME] [ERROR/WARNING/INFORMATION] - message_content
--

package V2P.Logs is

   type Module_Name is new String;

   type Log_Level is (Information, Warnings, Error);

   procedure Write
     (Name    : in Module_Name;
      Kind    : in Log_Level;
      Content : in String);
   pragma Inline (Write);
   --  Write Content into the log file

   procedure Set (Kind : in Log_Level; Activated : in Boolean);
   --  Activate/Deactivate the specified level of log

   function NV (Name, Value : in String) return String;
   function NV (Name : in String; Value : in Integer) return String;
   pragma Inline (NV);
   --  Returns a string formatted as Name="Value" for loggin purpose

end V2P.Logs;
