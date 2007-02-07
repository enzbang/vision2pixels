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

with AWS.Templates;

package V2P.Database is

   use AWS;

   type Filter_Mode is
     (Today, Two_Days, Seven_Days, Fifty_Messages, All_Messages);
   --  Kind of filter to apply when returning the list of posts, see
   --  Get_Threads.

   type Order_Direction is (DESC, ASC);

   function Get_Forums return Templates.Translate_Set;
   --  Returns the forum list

   function Get_Forum (Fid : in String) return String;
   --  Returns the DB line for the given forum

   function Get_Threads
     (Fid       : in String := "";
      User      : in String := "";
      From      : in Natural := 0;
      Filter    : in Filter_Mode := All_Messages;
      Order_Dir : in Order_Direction := DESC)
     return Templates.Translate_Set;
   --  Returns all threads for a given forum

   function Get_Thread_Navigation_Links
     (Fid       : in String;
      Tid       : in String;
      User      : in String := "";
      From      : in Natural := 0;
      Filter    : in Filter_Mode := All_Messages;
      Order_Dir : in Order_Direction := DESC)
     return Templates.Translate_Set;
   --  Returns next and previous thread (id and thumbnail)

   function Get_Categories (Fid : in String) return Templates.Translate_Set;
   --  Returns all categories for the given forum

   function Get_Category (Tid : in String) return Templates.Translate_Set;
   --  Returns the category for the given thread

   function Get_Category_Full_Name (CID : in String) return String;
   --  Returns a category name "Forum/Category" for the given id

   function Get_Entry (Tid : in String) return Templates.Translate_Set;
   --  Returns the full content of the entry Id

   function Get_Comment (Cid : in String) return Templates.Translate_Set;
   --  Returns a comment for the given comment id

   function Get_User (Uid : in String) return Templates.Translate_Set;
   --  Returns user's Id information

   function Get_Password (Uid : in String) return String;
   --  Returns the password for the given user. Returns the empty string if
   --  User cannot be found into the database.

   function Get_User_Tmp_Photo
     (Uid : in String) return Templates.Translate_Set;
   --  Returns user's temporaries photos

   function Get_Metadata (Pid : in String) return Templates.Translate_Set;
   --  Returns photo metadata


   function Insert_Comment
     (Uid       : in String;
      Anonymous : in String;
      Thread    : in String;
      Name      : in String;
      Comment   : in String;
      Pid       : in String)
     return String;
   --  Insert comment Name/Comment into the given forum and thread
   --  Returns Comment Id

   function Insert_Photo
     (Uid         : in String;
      Filename    : in String;
      Height      : in Integer;
      Width       : in Integer;
      Size        : in Integer)
     return String;
   --  Insert a new photo into the database
   --  Returns photo id

   function Insert_Post
     (Uid         : in String;
      Category_Id : in String;
      Name        : in String;
      Comment     : in String;
      Pid         : in String)
     return String;
   --  Insert a new post into the database
   --  Returns post id

   procedure Increment_Visit_Counter (Pid : in String);
   --  Increment a thread visit counter

   function Is_Author (Uid, Pid : in String) return Boolean;
   --  Returns true whether the user of the post Pid is Uid

end V2P.Database;
