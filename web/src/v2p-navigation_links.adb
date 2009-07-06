------------------------------------------------------------------------------
--                                Vision2Pixels                             --
--                                                                          --
--                           Copyright (C) 2007-2009                        --
--                        Pascal Obry - Olivier Ramonat                     --
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

with V2P.Context;
with V2P.Database;
with V2P.Template_Defs.Set_Global;

package body V2P.Navigation_Links is

   use AWS.Services.Web_Block.Context;

   Navigation_Links_Name : constant String := "Navigation_Links";

   package Links is
     new Generic_Data (Data      => Post_Ids.Vector,
                       Null_Data => Post_Ids.Empty_Vector);
   --  Adds Post_Ids.Vector to context value data

   procedure Get_Threads
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive;
      Mode         : in     Database.Select_Mode;
      Translations : in out Templates.Translate_Set);
   --  Internal version which handle all modes

   procedure Set_Navigation
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive);
   --  Set only the navigation into the context

   -----------------
   -- Get_Threads --
   -----------------

   procedure Get_Threads
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive;
      Mode         : in     Database.Select_Mode;
      Translations : in out Templates.Translate_Set)
   is
      use Template_Defs;
      Admin     : constant Boolean :=
                    Context.Exist (Template_Defs.Set_Global.ADMIN)
                  and then Context.Get_Value
                    (Template_Defs.Set_Global.ADMIN) = "TRUE";
      Nav_From  : Positive := From;
      Nav_Links : Post_Ids.Vector;
      Nb_Lines  : Natural;
      Total     : Natural;
   begin
      Database.Get_Threads
        (Fid         => V2P.Context.Counter.Get_Value
           (Context => Context.all,
            Name    => Set_Global.FID),
         From        => Nav_From,
         Admin       => Admin,
         Filter      => Database.Filter_Mode'Value
           (Context.Get_Value (Set_Global.FILTER)),
         Filter_Cat  => Context.Get_Value (Set_Global.FILTER_CATEGORY),
         Page_Size   => Page_Size,
         Order_Dir   => Database.Order_Direction'Value
           (Context.Get_Value (Set_Global.ORDER_DIR)),
         Sorting     => Database.Forum_Sort'Value
           (Context.Get_Value (Template_Defs.Set_Global.FORUM_SORT)),
         Mode        => Mode,
         Navigation  => Nav_Links,
         Set         => Translations,
         Nb_Lines    => Nb_Lines,
         Total_Lines => Total,
         TZ          => Context.Get_Value (Template_Defs.Set_Global.TZ));

      Links.Set_Value
        (Context => Context.all,
         Name    => Navigation_Links_Name,
         Value   => Nav_Links);

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Set_Global.NAV_FROM,
         Value   => Nav_From);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Set_Global.NAV_NB_LINES_RETURNED,
         Value   => Nb_Lines);

      V2P.Context.Counter.Set_Value
        (Context => Context.all,
         Name    => Set_Global.NAV_NB_LINES_TOTAL,
         Value   => Total);
   end Get_Threads;

   procedure Get_Threads
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive;
      Translations : in out Templates.Translate_Set) is
   begin
      Get_Threads
        (Context, Page_Size, From, Database.Everything, Translations);
   end Get_Threads;

   ------------------------
   -- Goto_Next_Previous --
   ------------------------

   procedure Goto_Next_Previous
     (Context : not null access Services.Web_Block.Context.Object;
      Moves   : in Integer)
   is
      From : Positive :=
               V2P.Context.Not_Null_Counter.Get_Value
                 (Context => Context.all,
                  Name    => Template_Defs.Set_Global.NAV_FROM);
   begin
      if From + Moves < 1 then
         From := 1;
      else
         From := From + Moves;
      end if;

      --  Update FROM counter

      V2P.Context.Not_Null_Counter.Set_Value
        (Context => Context.all,
         Name    => Template_Defs.Set_Global.NAV_FROM,
         Value   => From);
   end Goto_Next_Previous;

   ---------------
   -- Next_Post --
   ---------------

   function Next_Post
     (Context : access Services.Web_Block.Context.Object;
      Id      : in Positive) return Natural
   is
      use Post_Ids;
      use Template_Defs;

      Posts   : constant Vector :=
                  Links.Get_Value
                    (Context => Context.all, Name => Navigation_Links_Name);
      Current : Cursor := Find (Posts, Id);
   begin
      if Current = No_Element then
         return 0;
      end if;

      Next (Current);

      if Current /= No_Element then
         return Element (Current);

      else
         --  Try harder to find next post id

         Try_Harder : declare
            Page_Size : constant Navigation_Links.Page_Size :=
                          V2P.Context.Not_Null_Counter.Get_Value
                            (Context => Context.all,
                             Name    => Set_Global.FILTER_PAGE_SIZE);
            Total     : constant Natural :=
                          V2P.Context.Counter.Get_Value
                            (Context => Context.all,
                             Name    => Set_Global.NAV_NB_LINES_TOTAL);
            Nav_From  : constant Positive :=
                          V2P.Context.Not_Null_Counter.Get_Value
                            (Context => Context.all,
                             Name    => Set_Global.NAV_FROM);
         begin
            if Nav_From + Page_Size <= Total then
               --  Fetch more post ids

               Set_Navigation
                 (Context      => Context,
                  From         => Nav_From + Page_Size - 1,
                  Page_Size    => Page_Size * 2);

               --  Recursive call to Next_Post as the next element has been
               --  loaded in Links.

               return Next_Post (Context => Context, Id => Id);

            else
               --  End of post ids list. Abort

               return 0;
            end if;
         end Try_Harder;
      end if;
   end Next_Post;

   -------------------
   -- Previous_Post --
   -------------------

   function Previous_Post
     (Context : access Services.Web_Block.Context.Object;
      Id      : in Positive) return Natural
   is
      use Post_Ids;
      use Template_Defs;

      Posts   : constant Vector :=
                  Links.Get_Value
                    (Context => Context.all, Name => Navigation_Links_Name);
      Current : Cursor := Find (Posts, Id);
   begin
      if Current = No_Element then
         return 0;
      end if;

      Previous (Current);

      if Current /= No_Element then
         return Element (Current);

      else
         --  Try harder to find previous post id

         Try_Harder : declare
            Page_Size : constant Navigation_Links.Page_Size :=
                          V2P.Context.Not_Null_Counter.Get_Value
                            (Context => Context.all,
                             Name    => Set_Global.FILTER_PAGE_SIZE);
            Nav_From  : Positive :=
                          V2P.Context.Not_Null_Counter.Get_Value
                            (Context => Context.all,
                             Name    => Set_Global.NAV_FROM);
         begin
            if Nav_From /= 1 then

               if Nav_From > Page_Size then
                  Nav_From := Nav_From - Page_Size;
               else
                  Nav_From := 1;
               end if;

               --  Fetch more post ids

               Set_Navigation
                 (Context      => Context,
                  From         => Nav_From,
                  Page_Size    => Page_Size * 2);

               --  Recursive call to Previous_Post as the previous element
               --  has been loaded in Links.

               return Previous_Post (Context => Context, Id => Id);

            else
               --  Begin of post ids list. Abort

               return 0;
            end if;
         end Try_Harder;
      end if;
   end Previous_Post;

   --------------------
   -- Set_Navigation --
   --------------------

   procedure Set_Navigation
     (Context      : access Services.Web_Block.Context.Object;
      Page_Size    : in     Positive;
      From         : in     Positive)
   is
      Set : Templates.Translate_Set;
   begin
      Get_Threads
        (Context, Page_Size, From,
         Mode => Database.Navigation_Only, Translations => Set);
   end Set_Navigation;

end V2P.Navigation_Links;
