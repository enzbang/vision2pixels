------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2006-2009                          --
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

with V2P.Navigation_Links;

private with Ada.Task_Attributes;
private with DB;

package V2P.Database is

   use AWS;
   use Ada.Strings.Unbounded;

   No_Database    : exception;

   Database_Error :  exception;

   Parameter_Error : exception;
   --  Raise if the given parameter value does not exist in database

   type Filter_Mode is (Today, Two_Days, Seven_Days, All_Messages);
   --  Kind of filter to apply when returning the list of posts, see
   --  Get_Threads.

   subtype Id is Natural;
   Empty_Id : constant Id;

   function To_String (Id : in Database.Id) return String;
   pragma Inline (To_String);
   --  Returns Id image

   type Forum_Filter is (Forum_Text, Forum_Photo, Forum_All);

   type Forum_Type is new Forum_Filter range Forum_Text .. Forum_Photo;

   type Order_Direction is (DESC, ASC);

   type Forum_Sort is
     (Last_Posted, Last_Commented, Best_Noted, Need_Attention);

   type Image_Size is (Thumb_Size, Medium_Size, Max_Size);

   type User_Settings is record
      Page_Size  : Positive;
      Filter     : Filter_Mode;
      Sort       : Forum_Sort;
      Image_Size : Database.Image_Size;
   end record;

   type User_Data is record
      UID         : Unbounded_String;
      Password    : Unbounded_String;
      Admin       : Boolean;
      Preferences : User_Settings;
   end record;

   type Select_Mode is (Everything, Navigation_Only);

   No_User_Data : constant User_Data;

   function Get_Forums
     (Filter : in Forum_Filter) return Templates.Translate_Set;
   --  Returns the forum list
   --  If filter is Forum_Photo or Forum_Text and only one forum found, then
   --  returns the category list in that forum

   function Get_Forum (Fid, Tid : in Id) return Templates.Translate_Set;
   --  Returns the DB line for the given forum. If Fid is empty it uses the Tid
   --  information to get the corresponding forum Id.

   function Get_Forum_Id (Tid : in Id) return Id;
   --  Returns the FID corresponding to the given Tid

   function Get_Forum_Type (Tid : in Id) return V2P.Database.Forum_Type;
   --  Returns the forum type from a Tid

   procedure Get_Threads
     (Fid           : in     Id := Empty_Id;
      User          : in     String := "";
      Admin         : in     Boolean;
      Forum         : in     Forum_Filter := Forum_All;
      Page_Size     : in     Navigation_Links.Page_Size :=
        Navigation_Links.Default_Page_Size;
      Filter        : in     Filter_Mode := All_Messages;
      Filter_Cat    : in     String      := "";
      Order_Dir     : in     Order_Direction := DESC;
      Sorting       : in     Forum_Sort := Last_Posted;
      Only_Revealed : in     Boolean := False;
      From          : in out Positive;
      Mode          : in     Select_Mode := Everything;
      Navigation    :    out Navigation_Links.Post_Ids.Vector;
      Set           :    out Templates.Translate_Set;
      Nb_Lines      :    out Natural;
      Total_Lines   :    out Natural);
   --  Returns all threads for a given forum.
   --  Returns navigation links to store in context.
   --  Set Revealed to True to retreive only revealed photos.

   function Get_Latest_Posts
     (Limit    : in Positive;
      Admin    : in     Boolean;
      Add_Date : in Boolean := False) return Templates.Translate_Set;
   --  Returns the Limit latest posts from all photo based forums

   function Get_Latest_Users
     (Limit : in Positive) return Templates.Translate_Set;
   --  Returns the Limit latest registered users

   function Get_Thumbnail (Post : in Id) return String;
   --  Returns the thumbnail filename of the photo
   --  associated with the given post

   function Get_Categories (Fid : in Id) return Templates.Translate_Set;
   --  Returns all categories for the given forum

   function Get_Category (Tid : in Id) return Templates.Translate_Set;
   --  Returns the category for the given thread

   procedure Set_Category (Tid : in Id; Category_Id : in Id);
   --  Set the category of the given post

   function Get_Category_Full_Name (CID : in String) return String;
   --  Returns a category name "Forum/Category" for the given id

   function Get_Post
     (Tid        : in Id;
      Forum_Type : in V2P.Database.Forum_Type) return Templates.Translate_Set;
   --  Returns the post information (no comments)

   function Get_Entry
     (Tid        : in Id;
      Forum_Type : in V2P.Database.Forum_Type) return Templates.Translate_Set;
   --  Returns the full content of the entry Id. As above with comments

   function Get_Comment (Cid : in Id) return Templates.Translate_Set;
   --  Returns a comment for the given comment id

   function Get_Comments (Tid : in Id) return Templates.Translate_Set;
   --  Returns all comments for the given thread

   function Get_User_Data (Uid : in String) return User_Data;
   --  Returns the user's data. Returns the No_User_Data if User cannot be
   --  found into the database.

   function Get_Password_From_Email (Email : in String) return String;
   --  Returns the user's password from the given e-mail. Returns the empty
   --  string if the Email cannot be found into the database.

   procedure Set_Last_Logged (Uid : in String);
   --  Set last logged status in the database

   function Get_User_Comment
     (Uid     : in String;
      Limit   : in Positive;
      Textify : in Boolean := False) return Templates.Translate_Set;
   --  Returns user's comments

   function Get_User_Last_Photo
     (Uid : in String) return Templates.Translate_Set;
   --  Returns user's last photo (in the user photo queue)

   function Get_CdC return Templates.Translate_Set;
   --  Returns all CdC photos

   function Get_Metadata (Tid : in Id) return Templates.Translate_Set;
   --  Returns photo geographic metadata from thread if

   function Get_Exif (Tid : in Id) return Templates.Translate_Set;
   --  Returns photo exif metadata, get them from the image if needed and
   --  update the database.

   function Get_Photo_Of_The_Week return Templates.Translate_Set;
   --  Returns photo of the week

   function Toggle_Hidden_Status (Tid : in Id) return Templates.Translate_Set;
   --  Toggle Tid hidden status and returns the new status

   function Insert_Comment
     (Uid       : in String;
      Anonymous : in String;
      Thread    : in Id;
      Name      : in String;
      Comment   : in String;
      Pid       : in Id) return Id;
   --  Insert comment Name/Comment into the given forum and thread,
   --  Returns Comment Id.

   function Insert_Photo
     (Uid           : in String;
      Filename      : in String;
      Height        : in Integer;
      Width         : in Integer;
      Medium_Height : in Integer;
      Medium_Width  : in Integer;
      Thumb_Height  : in Integer;
      Thumb_Width   : in Integer;
      Size          : in Integer) return String;
   --  Insert a new photo into the database, returns photo id

   procedure Insert_Metadata
     (Pid                     : in Id;
      Geo_Latitude            : in Float;
      Geo_Longitude           : in Float;
      Geo_Latitude_Formatted  : in String;
      Geo_Longitude_Formatted : in String);
   --  Insert a new metadata into the database

   function Insert_Post
     (Uid         : in String;
      Category_Id : in Id;
      Name        : in String;
      Comment     : in String;
      Pid         : in Id) return Id;
   --  Insert a new post into the database. If Pid /= Empty_Id it is the photo
   --  Id for the new post, otherwise it is a textual post. returns post id.

   procedure Increment_Visit_Counter (Pid : in Id);
   --  Increment a thread visit counter

   function Is_Author (Uid : in String; Pid : in Id) return Boolean;
   --  Returns true whether the user of the post Pid is Uid

   function Get_User_Page (Uid : in String) return Templates.Translate_Set;
   --  Returns user page

   procedure Update_Page
     (Uid : in String; Content : in String; Content_HTML : in String);
   --  Update a user page

   procedure Update_Rating
     (Uid      : in String;
      Tid      : in Id;
      Criteria : in String;
      Value    : in String);
   --  Update post rating

   function Get_Global_Rating (Tid : in Id) return Templates.Translate_Set;
   --  Get the post global rating

   function Get_User_Rating_On_Post
     (Uid : in String; Tid : in Id) return Templates.Translate_Set;
   --  Get the user rating on a specific post

   function Get_New_Post_Delay
     (Uid : in String) return Templates.Translate_Set;
   --  Get the delay the user has to wait before he can post

   --  Weekly votes

   procedure Toggle_Vote_Week_Photo (Uid : in String; Tid : in Id);
   --  Set or Reset user Uid vote for photo Tid

   function Has_User_Vote (Uid : in String; Tid : in Id) return Boolean;
   --  Returns True if user Uid has voted for the given photo

   function Get_User_Voted_Photos
     (Uid : in String) return Templates.Translate_Set;
   --  Returns the translate table with the list of all voted photos for the
   --  given user.

   function Register_User (Login, Password, Email : in String) return Boolean;
   --  Register a new user before validation. Returns False if the user cannot
   --  be registered (duplicate login).

   function Validate_User (Login, Key : in String) return Boolean;
   --  Validate a registered user

   procedure User_Preferences
     (Login       : in     String;
      Preferences :    out User_Settings);
   --  Returns the user's preferences for the given user. If no preferences are
   --  set, use the default values.

   procedure Set_Filter_Preferences
     (Login  : in String;
      Filter : in Filter_Mode);
   --  Set filter preference for the given user

   procedure Set_Filter_Page_Size_Preferences
     (Login     : in String;
      Page_Size : in Positive);
   --  Set filter preference for the given user

   procedure Set_Filter_Sort_Preferences
     (Login : in String;
      Sort  : in Forum_Sort);
   --  Set sort preference for the given user

   procedure Set_Image_Size_Preferences
     (Login      : in String;
      Image_Size : in Database.Image_Size);
   --  Set image size preference for the given user

   function Get_Stats return Templates.Translate_Set;
   --  Returns global stats used in main page

   function Gen_Cookie (Login : String) return String;
   --  Generate a new random string for "remember me" authentication

   procedure Register_Cookie (Login : in String; Cookie : in String);
   --  Register a new cookie in database

   function Get_User_From_Cookie (Cookie : String) return String;
   --  Return user associated with the given cookie (or "")

private

   No_User_Data : constant User_Data :=
                    User_Data'
                      (Null_Unbounded_String,
                       Null_Unbounded_String,
                       False,
                       User_Settings'(1, All_Messages,
                                      Last_Commented, Max_Size));
   Empty_Id     : constant Id := 0;

   --  Connection

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   type TLS_DBH_Access is access all TLS_DBH;

   Null_DBH : constant TLS_DBH :=
                TLS_DBH'(Handle => null, Connected => False);

   package DBH_TLS is
     new Ada.Task_Attributes (Attribute => TLS_DBH, Initial_Value => Null_DBH);

   procedure Connect (DBH : in TLS_DBH_Access);
   --  Connect to the database if needed

end V2P.Database;
