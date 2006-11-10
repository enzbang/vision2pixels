------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                           Copyright (C) 2006                             --
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

generic

   type Parameter_Name is (<>);
   --  The list of parameter names to handle

   Max_Vector_Size : Positive := 1_024;
   --  Maximum size of a vector in the config file

package Config is

   --------
   -- IO --
   --------

   package IO is

      procedure Open (Config_File_Name : in String);

      procedure Close;
      --  Close and discard changes

      procedure Save_Close;
      --  Close and save changes

      Unknown_Parameter : exception;
      --  Raised when a parameter name is not known

      Uncomplete_Config : exception;
      --  Raised when some parameters are not specified in the config file

   end IO;

   --  Accessor functions for simple types

   function Get_Value (Parameter : in Parameter_Name) return String;
   function Get_Value (Parameter : in Parameter_Name) return Integer;
   function Get_Value (Parameter : in Parameter_Name) return Float;
   function Get_Value (Parameter : in Parameter_Name) return Boolean;

   --  To change the value of a parameter of simple types

   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in String);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Integer);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Float);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Boolean);

   --  Handle enumeration types

   generic
      type Enum is (<>);
   package Enum_Values is

      function Get_Value (Parameter : in Parameter_Name) return Enum;

      procedure Set_Value
        (Parameter : in Parameter_Name; Value : in Enum);

   end Enum_Values;

   --  Handle vectors parameters

   generic
      type T is private;
      type Vector is array (Positive range <>) of T;
      with procedure Get
        (From : in     String;                               --  String -> T
         Item :    out T;
         Last :    out Positive) is <>;
      with function Image (Item : in T) return String is <>; --  T -> String
   package Vector_Values is

      function Get_Value (Parameter : in Parameter_Name) return Vector;

      procedure Set_Value
        (Parameter : in Parameter_Name; Value : in Vector);

   end Vector_Values;

end Config;
