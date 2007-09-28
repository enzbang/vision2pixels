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

with V2P.Template_Defs.Forum_Entry;
with V2P.Template_Defs.Forum_Threads;
with V2P.Template_Defs.Main_Page;
with V2P.Template_Defs.Forum_New_Entry;
with V2P.Template_Defs.Global;
with V2P.Template_Defs.Post_Photo;
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
                      Parameters.Get (P, Template_Defs.Forum_Entry.HTTP.TID);
      Login       : constant String :=
                      Session.Get (SID, Template_Defs.Global.LOGIN);
      Count_Visit : Boolean := True;
   begin
      --  Set thread Id into the context

      Context.Set_Value (Template_Defs.Global.TID, TID);

      if TID /= "" then
         V2P.Context.Context_Filter (Context);

         if not Settings.Anonymous_Visit_Counter then
            --  Do not count anonymous click
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
                 (V2P.Template_Defs.Forum_Entry.PREVIOUS, Previous_Id));

            if Previous_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Forum_Entry.PREVIOUS_THUMB,
                     Database.Get_Thumbnail (Previous_Id)));
            end if;

            Templates.Insert
              (Translations, Templates.Assoc
                 (V2P.Template_Defs.Forum_Entry.NEXT, Next_Id));

            if Next_Id /= "" then
               Templates.Insert
                 (Translations, Templates.Assoc
                    (V2P.Template_Defs.Forum_Entry.NEXT_THUMB,
                     Database.Get_Thumbnail (Next_Id)));
            end if;
         end Insert_Links;

         --  Insert the entry information

         Templates.Insert (Translations, Database.Get_Entry (TID));
      end if;
   end Forum_Entry;

   -------------------
   -- Forum_Threads --
   -------------------

   procedure Forum_Threads
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Translations);

      P    : constant Parameters.List := Status.Parameters (Request);
      FID  : constant String :=
               Parameters.Get (P, Template_Defs.Forum_Threads.HTTP.FID);
      From : Positive := 1;
   begin
      --  Set forum Id into the context

      Context.Set_Value (Template_Defs.Global.FID, FID);

      if Context.Exist (Template_Defs.Global.TID) then
         Context.Remove (Template_Defs.Global.TID);
      end if;

      if Parameters.Exist
        (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM)
      then
         From := Positive'Value
           (Parameters.Get (P, Template_Defs.Block_Forum_Navigate.HTTP.FROM));
      end if;

      V2P.Context.Navigation_From.Set_Value
        (Context.all, Template_Defs.Global.NAV_FROM, From);

      V2P.Context.Context_Filter (Context);
   end Forum_Threads;

   ---------------
   -- Main_Page --
   ---------------

   procedure Main_Page
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Translations);
   begin
      if Context.Exist (Template_Defs.Global.TID) then
         Context.Remove (Template_Defs.Global.TID);
      end if;
      if Context.Exist (Template_Defs.Global.FID) then
         Context.Remove (Template_Defs.Global.FID);
      end if;

      V2P.Context.Context_Filter (Context);
   end Main_Page;

   procedure New_Entry_Page
     (Request      : in     Status.Data;
      Context      : access Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use Image.Data;

      P           : constant Parameters.List := Status.Parameters (Request);
      Filename    : constant String :=
                      Parameters.Get
                        (P, Template_Defs.Post_Photo.HTTP.FILENAME);
   begin

      --  If a new photo has been uploaded, insert it in database

      if Filename /= "" then
         New_Photo :
         declare
            SID         : constant Session.Id := Status.Session (Request);
            P           : constant Parameters.List :=
                            Status.Parameters (Request);
            Login       : constant String :=
                            Session.Get (SID, Template_Defs.Global.LOGIN);
            Filename    : constant String :=
                            Parameters.Get
                              (P, Template_Defs.Post_Photo.HTTP.FILENAME);
            New_Image   : Image_Data;

         begin
            Init (Img      => New_Image,
                  Root_Dir => Gwiad_Plugin_Path,
                  Filename => Filename);

            if New_Image.Init_Status /= Image_Created then
               Templates.Insert
                 (Translations,
                  Templates.Assoc (Template_Defs.Main_Page.V2P_ERROR,
                    Image_Init_Status'Image (New_Image.Init_Status)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Main_Page.EXCEED_MAXIMUM_IMAGE_DIMENSION,
                     Image_Init_Status'Image
                       (Image.Data.Exceed_Max_Image_Dimension)));

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Template_Defs.Main_Page.EXCEED_MAXIMUM_SIZE,
                     Image_Init_Status'Image
                       (Image.Data.Exceed_Max_Size)));

            else
               Insert_Photo : declare
                  use URL;
                  New_Photo_Filename : constant String := New_Image.Filename
                    (Images_Full_Prefix'Length + 1 .. New_Image.Filename'Last);
                  Pid                : constant String
                    := Database.Insert_Photo
                      (Login,
                       New_Photo_Filename,
                       Natural (New_Image.Dimension.Width),
                       Natural (New_Image.Dimension.Height),
                       Natural (New_Image.Dimension.Size));
               begin
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Forum_New_Entry.PID, Pid));
                  Templates.Insert
                    (Translations,
                     Templates.Assoc
                       (Template_Defs.Forum_New_Entry.IMAGE_SOURCE,
                        New_Photo_Filename));
               end Insert_Photo;
            end if;
         end New_Photo;
      end if;
   end New_Entry_Page;

end V2P.Callbacks.Page;
