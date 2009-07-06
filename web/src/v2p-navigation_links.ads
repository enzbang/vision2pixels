------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with Ada.Containers.Indefinite_Vectors;

with AWS.Services.Web_Block.Context;
with AWS.Templates;

package V2P.Navigation_Links is

   use Ada;
   use AWS;

   subtype Page_Size is Positive range 1 .. 500;
   --  Limit page size to 500

   Default_Page_Size : constant Page_Size;

   function Previous_Post
     (Context : access Services.Web_Block.Context.Object;
      Id      : in     Positive) return Natural;
   --  Returns previous post stored in Post_Ids.Vector

   function Next_Post
     (Context : access Services.Web_Block.Context.Object;
      Id      : in      Positive) return Natural;
   --  Returns next post stored in Post_Ids.Vector

   procedure Get_Threads
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive;
      Translations : in out Templates.Translate_Set);
   --  Get the threads given the current context and parameters. Set the
   --  navigation links.

   package Post_Ids is new Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Positive);
   --  Post_Ids stores all visible post ids in forum threads page

   procedure Goto_Next_Previous
     (Context : not null access Services.Web_Block.Context.Object;
      Moves   : in Integer);
   --  Move context variable NAV_FROM to next/previous page using Moves number

private

   Default_Page_Size : constant Page_Size := 10;

end V2P.Navigation_Links;
