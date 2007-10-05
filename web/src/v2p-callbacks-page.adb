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

with AWS.Parameters;
with AWS.Session;

with V2P.Context;
with V2P.Database;
with V2P.Settings;
with V2P.URL;

with V2P.Template_Defs.Page_Forum_Entry;
with V2P.Template_Defs.Page_Forum_Threads;
with V2P.Template_Defs.Page_Main;
with V2P.Template_Defs.Page_Forum_New_Entry;
with V2P.Template_Defs.Set_Global;
with V2P.Template_Defs.Page_Photo_Post;
with V2P.Template_Defs.Block_Forum_Navigate;

with Image.Data;

package body V2P.Callbacks.Page is

   -----------------
   -- Forum_Entry --
   -----------------

   procedure Forum_Entry
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID         : constant Session.Id := Status.Session (Request);
      P           : constant Parameters.List := Status.Parameters (Request);
      TID         : constant String :=
                      Parameters.Get
                        (P, Template_Defs.Page_Forum_Entry.HTTP.TID);
      Login       : constant String :=
                      Session.Get (SID, Template_Defs.Set_Global.LOGIN);
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the context

      Context.Set_Value (Template_Defs.Set_Global.TID, TID);

      if TID /= "" then
         V2P.Context.Context_Filter (Context);

         if not Settings.Anonymous_Visit_Counter then
            --  Do not count anonymous click
            --  ??? can use a simple assignment
            if Login = "" then
               Count_Visit := False;

            else
               if Settings.Ignore_Author_Click
                 and then Database.Is_Author (Login, TID)
               then
                  --  Do not count author click
                  Count_Visit := False;
               end if;
            end if;
         end if;

         if Count_Visit then
            Database.Increment_Visit_Counter (TID);
         end if;

         --  Insert navigation links (previous and next post)

         Insert_Links : declare
            Selected_Post : constant V2P.Context.Post_Ids.Vector :=
                              V2P.Context.Navigation_Links.Get_Value
                                (Context.all, "Navigation_Links");
            Previous_Id   : constant String :=
                              V2P.Context.Previous (Selected_Post, TID);
            Next_Id       : constant String :=
                              V2P.Context.Next (Selected_Post, TID);
         begin
            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Page_Forum_Entry.PREVIOUS, Previous_Id));

            if Previous_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Page_Forum_Entry.PREVIOUS_THUMB,
                     Database.Get_Thumbnail (Previous_Id)));
            end if;

            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Page_Forum_Entry.NEXT, Next_Id));

            if Next_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Page_Forum_Entry.NEXT_THUMB,
                     Database.Get_Thumbnail (Next_Id)));
            end if;
         end Insert_Links;

         --  Insert the entry information

         Templates.Insert (Translations, Database.Get_Entry (TID));
      end if;

      --  Add forum information into the translate set
      --  ??? we could probably cache those values

      Templates.Insert
        (Translations,
         Database.Get_Forum
           (Context.Get_Value (Template_Defs.Set_Global.FID), TID));
   end Forum_Entry;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      P    : constant Parameters.List := Status.Parameters (Request);
      FID  : constant String :=
               Parameters.Get (P, Template_Defs.Page_Forum_Threads.HTTP.FID);
      From : Positive := 1;
   begin
      --  Set forum Id into the context

      Context.Set_Value (Template_Defs.Set_Global.FID, FID);

      if Context.Exist (Template_Defs.Set_Global.TID) then
         Context.Remove (Template_Defs.Set_Global.TID);
      end if;

      if Parameters.Exist
        (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM)
      then
         From := Positive'Value
           (Parameters.Get (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM));
      end if;

      V2P.Context.Navigation_From.Set_Value
        (Context.all, Template_Defs.Set_Global.NAV_FROM, From);

      Templates.Insert (Translations, Database.Get_Forum (FID, Tid => ""));

      V2P.Context.Context_Filter (Context);
   end Forum_Threads;

   ----------
   -- Main --
   ----------

   procedure Main
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist (Template_Defs.Set_Global.TID) then
         Context.Remove (Template_Defs.Set_Global.TID);
      end if;

      if Context.Exist (Template_Defs.Set_Global.FID) then
         Context.Remove (Template_Defs.Set_Global.FID);
      end if;

      V2P.Context.Context_Filter (Context);
   end Main;

   --------------------
   -- New_Entry_Page --
   --------------------

   procedure New_Entry
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use Image.Data;

      P        : constant Parameters.List := Status.Parameters (Request);
      Filename : constant String :=
                   Parameters.Get
                     (P, Template_Defs.Page_Photo_Post.HTTP.FILENAME);

      SID      : constant Session.Id := Status.Session (Request);
      Login    : constant String :=
                   Session.Get (SID, Template_Defs.Set_Global.LOGIN);

   begin
      --  If a new photo has been uploaded, insert it in database

      if Filename /= "" then
         New_Photo :
         declare
            New_Image : Image_Data;
         begin
            Init (Img      => New_Image,
                  Root_Dir => Gwiad_Plugin_Path,
                  Filename => Filename);

            if New_Image.Init_Status /= Image_Created then
               Templates.Insert
                 (Translations,
                  Templates.Assoc (Template_Defs.Page_Main.V2P_ERROR,
                    Image_Init_Status'Image (New_Image.Init_Status)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Page_Main.EXCEED_MAXIMUM_IMAGE_DIMENSION,
                     Image_Init_Status'Image
                       (Image.Data.Exceed_Max_Image_Dimension)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Page_Main.EXCEED_MAXIMUM_SIZE,
                     Image_Init_Status'Image
                       (Image.Data.Exceed_Max_Size)));

            else
               Insert_Photo : declare
                  use URL;
                  New_Photo_Filename : constant String := New_Image.Filename
                    (Images_Full_Prefix'Length + 1 .. New_Image.Filename'Last);
                  Pid                : constant String
                    := Database.Insert_Photo
                      (Uid      => Login,
                       Filename => New_Photo_Filename,
                       Height   => Natural (New_Image.Dimension.Height),
                       Width    => Natural (New_Image.Dimension.Width),
                       Size     => Natural (New_Image.Dimension.Size));
               begin
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Page_Forum_New_Entry.PID, Pid));
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Page_Forum_New_Entry.IMAGE_SOURCE,
                        New_Photo_Filename));
               end Insert_Photo;
            end if;
         end New_Photo;
      else
         if Context.Exist (Template_Defs.Set_Global.HAS_POST_PHOTO) then
            --  Display last uploaded photo

            Templates.Insert
              (Translations,
               Database.Get_User_Last_Photo (Login));
            Context.Remove (Template_Defs.Set_Global.HAS_POST_PHOTO);
         end if;
      end if;
   end New_Entry;

   ----------------
   -- Post_Photo --
   ----------------

   procedure Post_Photo
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      SID   : constant Session.Id := Status.Session (Request);
      Login : constant String :=
                Session.Get (SID, Template_Defs.Set_Global.LOGIN);
   begin
      if Login /= "" then
         if not Context.Exist (Template_Defs.Set_Global.HAS_POST_PHOTO) then
            Context.Set_Value (Template_Defs.Set_Global.HAS_POST_PHOTO,
                               Boolean'Image (True));
         end if;
         Templates.Insert
           (Translations,
            Database.Get_User_Last_Photo (Login));
      end if;
   end Post_Photo;

end V2P.Callbacks.Page;
