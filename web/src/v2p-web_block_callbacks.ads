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

package V2P.Web_Block_Callbacks is

   use AWS;
   use AWS.Services;

   procedure Exif
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Forum_Filter
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Forum_List
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Forum_List_Select
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Forum_Threads
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Login
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Metadata
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure New_Comment
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure New_Photo
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set) is null;

   procedure New_Post
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure Quick_Login
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set) is null;

   procedure User_Page
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure User_Comment_List
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure User_Thread_List
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   procedure User_Tmp_Photo_Select
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

end V2P.Web_Block_Callbacks;
