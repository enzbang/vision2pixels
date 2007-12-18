------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                          Copyright (C) 2007                           --
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

with Morzhol.OS;

with V2P.Settings;

package body V2P.Cache is

   use Ada;

   Cache_Ext : constant String := ".cached";
   --  Cache filename extension

   -----------
   -- Clear --
   -----------

   procedure Clear (Root_Directory : in String) is

      use Directories;

      procedure Delete (Directory_Entry : in Directory_Entry_Type);
      --  Delete the directory entry

      ------------
      -- Delete --
      ------------

      procedure Delete (Directory_Entry : in Directory_Entry_Type) is
         SN : constant String := Simple_Name (Directory_Entry);
      begin
         if Kind (Directory_Entry) = Directory
           and then SN /= ".." and then SN /= "."
         then
            Clear (Compose (Root_Directory, SN));

         elsif '.' & Extension (SN) = Cache_Ext then
            Delete_File (Directories.Full_Name (Directory_Entry));
         end if;
      end Delete;

   begin
      Search
        (Root_Directory,
         Pattern => "*",
         Filter  =>
           Filter_Type'(Ordinary_File | Directory => True, others => False),
         Process => Delete'Access);
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create (Filename, Content : in String) is
      C_File : constant String := Name (Filename);
      C_Dir  : constant String := Directories.Containing_Directory (C_File);
      File   : Text_IO.File_Type;
   begin
      Directories.Create_Path (C_Dir);
      Text_IO.Create (File, Text_IO.Out_File, C_File);
      Text_IO.Put_Line (File, Content);
      Text_IO.Close (File);
   end Create;

   ----------
   -- Name --
   ----------

   function Name (Filename : in String) return String is

      function Clean_Filename return String;
      pragma Inline (Clean_Filename);
      --  Returns Filename with translated drive separator on Windows. This
      --  filename must be correct if appended to a prefix.

      --------------------
      -- Clean_Filename --
      --------------------

      function Clean_Filename return String is
      begin
         if Filename (Filename'First + 1) = ':' then
            return Filename (Filename'First)
              & "_" & Filename (Filename'First + 2 .. Filename'Last);
         else
            return Filename;
         end if;
      end Clean_Filename;

   begin
      return Settings.Cache_Path
        & Morzhol.OS.Directory_Separator & Clean_Filename & Cache_Ext;
   end Name;

end V2P.Cache;
