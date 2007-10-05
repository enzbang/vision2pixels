------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
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

with Ada.Strings.Unbounded;

with AWS.Templates;
with V2P.Context;

package V2P.Database is

   use AWS;
   use Ada.Strings.Unbounded;

   No_Database : exception;

   type Filter_Mode is
     (Today, Two_Days, Seven_Days, Fifty_Messages, All_Messages);
   --  Kind of filter to apply when returning the list of posts, see
   --  Get_Threads.

   type User_Data is record
      UID      : Unbounded_String;
      Password : Unbounded_String;
      Admin    : Boolean;
   end record;

   No_User_Data : constant User_Data;

   type Order_Direction is (DESC, ASC);

   function Get_Forums return Templates.Translate_Set;
   --  Returns the forum list

   function Get_Forum (Fid, Tid : in String) return Templates.Translate_Set;
   --  Returns the DB line for the given forum. If Fid is empty it uses the Tid
   --  information to get the corresponding forum Id.

   procedure Get_Threads
     (Fid        : in     String := "";
      User       : in     String := "";
      Admin      : in     Boolean;
      From       : in     Positive := 1;
      Filter     : in     Filter_Mode := All_Messages;
      Order_Dir  : in     Order_Direction := DESC;
      Navigation :    out V2P.Context.Post_Ids.Vector;
      Set        :    out Templates.Translate_Set);
   --  Returns all threads for a given forum
   --  Returns navigation links to store in context

   function Get_Thumbnail (Post : in String) return String;
   --  Returns the thumbnail filename of the photo
   --  associated with the given post

   function Get_Categories (Fid : in String) return Templates.Translate_Set;
   --  Returns all categories for the given forum

   function Get_Category (Tid : in String) return Templates.Translate_Set;
   --  Returns the category for the given thread

   function Get_Category_Full_Name (CID : in String) return String;
   --  Returns a category name "Forum/Category" for the given id

   function Get_Post (Tid : in String) return Templates.Translate_Set;
   --  Returns the post information (no comments)

   function Get_Entry (Tid : in String) return Templates.Translate_Set;
   --  Returns the full content of the entry Id. As above with comments

   function Get_Comment (Cid : in String) return Templates.Translate_Set;
   --  Returns a comment for the given comment id

   function Get_User (Uid : in String) return Templates.Translate_Set;
   --  Returns user's Id information

   function Get_User_Data (Uid : in String) return User_Data;
   --  Returns the user's data. Returns the No_User_Data if User cannot be
   --  found into the database.

   function Get_User_Comment
     (Uid : in String; Textify : in Boolean := False)
      return Templates.Translate_Set;
   --  Returns user's comments

   function Get_User_Last_Photo
     (Uid : in String) return Templates.Translate_Set;
   --  Returns user's last photo (in the user photo queue)

   function Get_Metadata (Pid : in String) return Templates.Translate_Set;
   --  Returns photo geographic metadata

   function Get_Exif (Pid : in String) return Templates.Translate_Set;
   --  Returns photo exif metadata, get them from the image if needed and
   --  update the database.

   function Toggle_Hidden_Status
     (Tid : in String) return Templates.Translate_Set;
   --  Toggle Tid hidden status and returns the new status

   function Insert_Comment
     (Uid       : in String;
      Anonymous : in String;
      Thread    : in String;
      Name      : in String;
      Comment   : in String;
      Pid       : in String) return String;
   --  Insert comment Name/Comment into the given forum and thread,
   --  Returns Comment Id.

   function Insert_Photo
     (Uid      : in String;
      Filename : in String;
      Height   : in Integer;
      Width    : in Integer;
      Size     : in Integer) return String;
   --  Insert a new photo into the database
   --  Returns photo id

   procedure Insert_Metadata
     (Pid                     : in String;
      Geo_Latitude            : in Float;
      Geo_Longitude           : in Float;
      Geo_Latitude_Formatted  : in String;
      Geo_Longitude_Formatted : in String);
   --  Insert a new metadata into the database

   function Insert_Post
     (Uid         : in String;
      Category_Id : in String;
      Name        : in String;
      Comment     : in String;
      Pid         : in String) return String;
   --  Insert a new post into the database and returns post id

   procedure Increment_Visit_Counter (Pid : in String);
   --  Increment a thread visit counter

   function Is_Author (Uid, Pid : in String) return Boolean;
   --  Returns true whether the user of the post Pid is Uid

   function Get_User_Page (Uid : in String) return Templates.Translate_Set;
   --  Returns user page

   procedure Update_Page
     (Uid : in String; Content : in String; Content_HTML : in String);
   --  Update a user page

   procedure Update_Rating
     (Uid      : in String;
      Tid      : in String;
      Criteria : in String;
      Value    : in String);
   --  Update post rating

   function Get_Global_Rating (Tid : in String) return Templates.Translate_Set;
   --  Get the post global rating

   function Get_User_Rating_On_Post
     (Uid : in String; Tid : in String) return Templates.Translate_Set;
   --  Get the user rating on a specific post

private

   No_User_Data : constant User_Data := User_Data'
     (Null_Unbounded_String, Null_Unbounded_String, False);

end V2P.Database;
