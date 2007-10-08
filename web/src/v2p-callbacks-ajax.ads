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

package V2P.Callbacks.Ajax is

   use AWS;

   procedure Login
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Login callback

   procedure Logout
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Logout callback

   procedure Onchange_Forum_List
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when a new forum is selected on post page

   procedure Onchange_Filter_Forum
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when changing the forum sorting

   procedure Onclick_Hidden_Status_Toggle
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when changing the forum sorting

   procedure Onsubmit_Comment_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting a new comment

   procedure Onsubmit_Metadata_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting new metadata

   procedure Onsubmit_Post_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting a new post

   procedure Onsubmit_User_Page_Edit_Form_Enter
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting new user page content

   procedure Onsubmit_Rate
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Called when submitting a new rating

   procedure On_Context_Error
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set) is null;
   --  Context error callback

end V2P.Callbacks.Ajax;
