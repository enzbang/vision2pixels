------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Morzhol.OS;
with ZLib;

with V2P.Settings;

package body V2P.Cache is

   use Ada;
   use Ada.Streams;

   Cache_Ext    : constant String := ".cached";
   --  Cache filename extension

   Compress_Ext : constant String := ".gz";
   --  Extension for compressed cache files

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
            Clear (Compose (Containing_Directory => Root_Directory,
                            Name                 => SN));

         elsif '.' & Extension (SN) = Cache_Ext then
            Delete_File (Directories.Full_Name (Directory_Entry));
         end if;
      end Delete;

   begin
      Search
        (Root_Directory,
         Pattern => "*",
         Filter  =>
           Filter_Type'(Ordinary_File | Directory => True,
                        Special_File              => False),
         Process => Delete'Access);
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create (Filename, Content : in String; Compress : in Boolean) is
      C_File : constant String := Name (Filename);
      C_Dir  : constant String := Directories.Containing_Directory (C_File);
      File   : Text_IO.File_Type;
   begin
      --  Create full path if needed

      Directories.Create_Path (C_Dir);

      --  Write file

      Text_IO.Create (File => File, Mode => Text_IO.Out_File, Name => C_File);
      Text_IO.Put_Line (File, Content);
      Text_IO.Close (File);

      if Compress then
         Zip_File : declare
            File_In         : Stream_IO.File_Type;
            Compressed_File : Stream_IO.File_Type;
            Filter          : ZLib.Filter_Type;

            procedure Data_In
              (Item : out Stream_Element_Array;
               Last : out Stream_Element_Offset);
            --  Read data from File

            procedure Data_Out (Item : in Stream_Element_Array);
            --  Write data to the Compress_File

            procedure Translate is new ZLib.Generic_Translate
              (Data_In  => Data_In,
               Data_Out => Data_Out);

            -------------
            -- Data_In --
            -------------

            procedure Data_In
              (Item : out Stream_Element_Array;
               Last : out Stream_Element_Offset) is
            begin
               Stream_IO.Read (File_In, Item, Last);
            end Data_In;

            --------------
            -- Data_Out --
            --------------

            procedure Data_Out (Item : in Stream_Element_Array) is
            begin
               Stream_IO.Write (Compressed_File, Item);
            end Data_Out;

         begin
            Stream_IO.Open (File_In, Stream_IO.In_File, C_File);
            Stream_IO.Create
              (Compressed_File, Stream_IO.Out_File, C_File & Compress_Ext);

            --  Deflate File

            ZLib.Deflate_Init
              (Filter => Filter,
               Level  => ZLib.Best_Compression,
               Header => ZLib.GZip);

            Translate (Filter);
            ZLib.Close (Filter);

            Stream_IO.Close (File_In);
            Stream_IO.Close (Compressed_File);
         end Zip_File;
      end if;
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

   ---------------------
   -- Name_Compressed --
   ---------------------

   function Name_Compressed (Filename : in String) return String is
   begin
      return Name (Filename) & Compress_Ext;
   end Name_Compressed;

end V2P.Cache;
