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

package body AWS.Services.ECWF.Context is

   ------------
   -- Create --
   ------------

   function Create return Id is
   begin
      return Id (Session.Create);
   end Create;

   -----------
   -- Exist --
   -----------

   function Exist (CID : in Id) return Boolean is
   begin
      return Session.Exist (Session.Id (CID));
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (CID : in Id) return Object is
   begin
      return Object'(SID => Session.Id (CID));
   end Get;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Context : in Object; Name : in String) return String is
   begin
      return Session.Get (Context.SID, Name);
   end Get_Value;

   -----------
   -- Image --
   -----------

   function Image (CID : in Id) return String is
   begin
      return Session.Image (Session.Id (CID));
   end Image;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Context : in out Object; Name, Value : in String) is
   begin
      Session.Set (Context.SID, Name, Value);
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value (CID : in String) return Id is
   begin
      return Id (Session.Value (CID));
   exception
      when Constraint_Error =>
         return Id (Session.No_Session);
   end Value;

end AWS.Services.ECWF.Context;
