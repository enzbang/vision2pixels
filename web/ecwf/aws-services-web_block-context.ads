------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2007                            --
--                                 AdaCore                                  --
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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with AWS.Session;

package AWS.Services.Web_Block.Context is

   type Object is tagged private;
   --  A context object, can be used to record key/name values

   type Id is private;

   function Image (CID : in Id) return String;
   --  Returns CID string representation

   function Value (CID : in String) return Id;
   --  Returns Id given it's string representation

   function Create return Id;
   --  Creates a new context and returns the corresponding Id

   function Exist (CID : in Id) return Boolean;
   --  Returns True if CID context exists into the database

   function Get (CID : in Id) return Object;
   --  Returns the context object corresponding to CID

   procedure Set_Value (Context : in out Object; Name, Value : in String);
   --  Adds a new name/value pair

   function Get_Value (Context : in Object; Name : in String) return String;
   --  Gets the value for the key Name

private

   use AWS;

   type Id is new Session.Id;

   type Object is tagged record
      SID : Session.Id;
   end record;

end AWS.Services.Web_Block.Context;
