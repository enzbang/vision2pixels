------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with AUnit.Test_Cases.Registration;
with AUnit.Assertions;
with Ada.Directories;

with G2F;
with Image.Data;
with Image.Config;

use AUnit.Test_Cases.Registration;
use AUnit.Assertions;

package body Image_Tests.Thumbnails is

   procedure Create_Thumbnail (T : in out AUnit.Test_Cases.Test_Case'Class);
   --  Create Thumbnails

   ----------------------
   -- Create_Thumbnail --
   ----------------------

   procedure Create_Thumbnail (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      In_Filename : constant String := "adapowered.jpg";
      Category    : constant String := "Test";
      Test_Image  : Image.Data.Image_Data;

      Out_Directory : constant String :=
         Ada.Directories.Compose (Image.Config.Thumbs_Path, Category);
      Out_Filename  : constant String :=
         Ada.Directories.Compose (Out_Directory, In_Filename);
   begin

      --  Read image info and create thumbnail

      Image.Data.Init (Test_Image, In_Filename, Category);

      Assert
        (Ada.Directories.Exists (Out_Filename),
         Out_Filename & "does not exist");

      G2F.Destroy_Magick;
   end Create_Thumbnail;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return String_Access is
      pragma Unreferenced (T);
   begin
      return new String'("Create image thumbnails");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Create_Thumbnail'Access, "Create Thumbnails");
      null;
   end Register_Tests;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      G2F.Destroy_Magick;
   end Tear_Down;


end Image_Tests.Thumbnails;
