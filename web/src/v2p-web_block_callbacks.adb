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

with V2P.URL;
with V2P.Database;
with V2P.Context;
with V2P.Template_Defs.Block_New_Comment;
with V2P.Template_Defs.Block_User_Page;
with V2P.Template_Defs.Global;

package body V2P.Web_Block_Callbacks is

   ----------
   -- Exif --
   ----------

   procedure Exif
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist ("TID") then
         Templates.Insert
           (Translations,
            Database.Get_Exif (Context.Get_Value ("TID")));
      end if;
   end Exif;

   ------------------
   -- Forum_Filter --
   ------------------

   procedure Forum_Filter
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      Templates.Insert
        (Translations,
         Templates.Assoc
           (Template_Defs.Global.FILTER,
            Context.Get_Value (Template_Defs.Global.FILTER)));
   end Forum_Filter;

   ----------------
   -- Forum_List --
   ----------------

   procedure Forum_List
     (Request      : in     Status.Data;
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
     (Request      : in     Status.Data;
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
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use V2P.Context;

      SID       : constant Session.Id := Status.Session (Request);
      Admin     : constant Boolean :=
                    Session.Exist (SID, Template_Defs.Global.ADMIN)
                  and then Session.Get (SID, Template_Defs.Global.ADMIN);
      Set       : Templates.Translate_Set;
      Nav_Links : V2P.Context.Post_Ids.Vector;
   begin
      Database.Get_Threads
        (FID        => Context.Get_Value (Template_Defs.Global.FID),
         From       => Navigation_From.Get_Value
           (Context.all, Template_Defs.Global.NAV_FROM),
         Admin      => Admin,
         Filter     => Database.Filter_Mode'Value (Context.Get_Value
           (Template_Defs.Global.FILTER)),
         Order_Dir  => Database.Order_Direction'Value
           (Context.Get_Value (Template_Defs.Global.ORDER_DIR)),
         Navigation => Nav_Links,
         Set        => Set);

      V2P.Context.Navigation_Links.Set_Value
        (Context.all, "Navigation_Links", Nav_Links);

      Templates.Insert (Translations, Set);
   end Forum_Threads;

   -----------
   -- Login --
   -----------

   procedure Login
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User (Session.Get (SID, Template_Defs.Global.LOGIN)));
   end Login;

   --------------
   -- Metadata --
   --------------

   procedure Metadata
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist ("TID") then
         if Context.Exist
           (V2P.Template_Defs.Global.ERROR_METADATA_NULL_METADATA) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (V2P.Template_Defs.Global.ERROR_METADATA_NULL_METADATA,
                  "ERROR"));
            Context.Remove
              (V2P.Template_Defs.Global.ERROR_METADATA_NULL_METADATA);

         elsif Context.Exist
           (V2P.Template_Defs.Global.ERROR_METADATA_UNKNOWN_PHOTO) then
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (V2P.Template_Defs.Global.ERROR_METADATA_UNKNOWN_PHOTO,
                  "ERROR"));
            Context.Remove
              (V2P.Template_Defs.Global.ERROR_METADATA_UNKNOWN_PHOTO);

         else
            Templates.Insert
              (Translations,
               Database.Get_Metadata (Context.Get_Value ("TID")));
         end if;
      end if;
   end Metadata;

   -----------------
   -- New_Comment --
   -----------------

   procedure New_Comment
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request);
   begin
      if Context.Exist ("FID") then
         Templates.Insert
           (Translations,
            Database.Get_Categories (Context.Get_Value ("FID")));
      end if;

      if Context.Exist ("TID") then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Block_New_Comment.Current_TID,
               Context.Get_Value ("TID")));
      end if;
   end New_Comment;

   --------------
   -- New_Post --
   --------------

   procedure New_Post
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist ("TID") then
         Context.Remove ("TID");
      end if;
   end New_Post;

   -----------------
   -- Quick_Login --
   -----------------

   procedure Quick_Login
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      if Session.Exist (SID, Template_Defs.Global.LOGIN) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Global.LOGIN,
               String'(Session.Get (SID, Template_Defs.Global.LOGIN))));
      end if;
   end Quick_Login;

   -----------------------
   -- User_Comment_List --
   -----------------------

   procedure User_Comment_List
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User_Comment (Uid => User_Name, Textify => True));
   end User_Comment_List;

   ---------------
   -- User_Page --
   ---------------

   procedure User_Page
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      URI       : constant String := Status.URI (Request);
      User_Name : constant String := URL.User_Name (URI);

   begin
      Templates.Insert
        (Translations, Database.Get_User_Page (Uid => User_Name));

      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Block_User_Page.USER_NAME, User_Name));
   end User_Page;

   ----------------------
   -- User_Thread_List --
   ----------------------

   procedure User_Thread_List
     (Request      : in     Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);

      SID        : constant Session.Id := Status.Session (Request);
      Admin      : constant Boolean :=
                     Session.Exist (SID, Template_Defs.Global.ADMIN)
                    and then Session.Get (SID, Template_Defs.Global.ADMIN);
      URI        : constant String     := Status.URI (Request);
      User_Name  : constant String     := URL.User_Name (URI);
      Set        : Templates.Translate_Set;
      Navigation : V2P.Context.Post_Ids.Vector;

   begin
      Database.Get_Threads
        (User       => User_Name,
         Navigation => Navigation,
         Set        => Set,
         Admin      => Admin);

      Templates.Insert (Translations, Set);
   end User_Thread_List;

   ---------------------------
   -- User_Tmp_Photo_Select --
   ---------------------------

   procedure User_Tmp_Photo_Select
     (Request      : in Status.Data;
      Context      : access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      SID : constant Session.Id := Status.Session (Request);
   begin
      Templates.Insert
        (Translations,
         Database.Get_User_Tmp_Photo (Uid => Session.Get (SID, "LOGIN")));
   end User_Tmp_Photo_Select;

end V2P.Web_Block_Callbacks;
