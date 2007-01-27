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

with AWS.Status;
with AWS.Templates;

with AWS.Services.Web_Block.Context;

package AWS.Services.Web_Block.Registry is

   type Lazy_Handler is new Templates.Dynamic.Lazy_Tag with record
      Request      : Status.Data;
      --  Current request made to the server
      Translations : Templates.Translate_Set;
      --  Global translations table
   end record;

   procedure Register
     (Tag      : in String;
      Template : in String;
      Data_CB  : not null access procedure
        (Request      : in Status.Data;
         Context      : access Web_Block.Context.Object;
         Translations : in out Templates.Translate_Set));

private

   overriding
   procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : in              String;
      Translations : in out          Templates.Translate_Set);

end AWS.Services.Web_Block.Registry;
