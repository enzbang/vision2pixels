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

with AWS.Status;
with AWS.Templates;
with AWS.Services.Web_Block.Context;

package V2P.Callbacks.Page is

   use AWS;

   procedure Main
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Display v2p main page

   procedure Forum_Entry
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum entry callback

   procedure Forum_Threads
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Forum threads callback

   procedure New_Entry
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  New entry callback

   procedure Post_Photo
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Post photo callback

end V2P.Callbacks.Page;
