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

with AWS.Session;

with V2P.Database;
with V2P.Template_Defs.Block_Forum_Filter;
with V2P.Template_Defs.Block_New_Comment;

package body V2P.Block_Callbacks is

   ------------------
   -- Forum_Filter --
   ------------------

   procedure Forum_Filter
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Block_Forum_Filter.HTTP.Filter,
            String'(Session.Get (SID, "FILTER"))));
   end Forum_Filter;

   ----------------
   -- Forum_List --
   ----------------

   procedure Forum_List
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert (Translations, Database.Get_Forums);
   end Forum_List;

   -----------------------
   -- Forum_List_Select --
   -----------------------

   procedure Forum_List_Select
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
   begin
      Templates.Insert (Translations, Database.Get_Forums);
   end Forum_List_Select;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      if Session.Get (SID, "FID") /= "" then
         Templates.Insert
           (Translations,
            Database.Get_Threads
              (Fid    => Session.Get (SID, "FID"),
               Filter => Database.Filter_Mode'Value
                 (Session.Get (SID, "FILTER")),
               Order_Dir => Database.Order_Direction'Value
                 (Session.Get (SID, "ORDER_DIR"))));
      end if;
   end Forum_Threads;

   -----------
   -- Login --
   -----------

   procedure Login
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Templates.Insert
        (Translations, Database.Get_User (Session.Get (SID, "LOGIN")));
   end Login;

   -----------------
   -- New_Comment --
   -----------------

   procedure New_Comment
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      if Session.Exist (SID, "FID") then
         Templates.Insert
           (Translations,
            Database.Get_Categories (Session.Get (SID, "FID")));
      end if;

      if Session.Exist (SID, "TID") then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_New_Comment.Current_Tid,
               String'(Session.Get (SID, "TID"))));
      end if;
   end New_Comment;

   --------------
   -- New_Post --
   --------------

   procedure New_Post
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context, Translations);
   begin
      null;
   end New_Post;

   -----------------
   -- Quick_Login --
   -----------------

   procedure Quick_Login
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context, Translations);
   begin
      null;
   end Quick_Login;

   ----------------------
   -- User_Thread_List --
   ----------------------

   procedure User_Thread_List
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_Threads (User => Session.Get (SID, "LOGIN")));
   end User_Thread_List;

end V2P.Block_Callbacks;
