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

with Ada.IO_Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Config is

   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   Max_Line_Length : constant := 1_024;

   subtype Buffer_String is String (1 .. Max_Line_Length);

   Parameters : array (Parameter_Name) of Unbounded_String;

   --------------------------------------------------------------------------

   package body IO is

      package Parameter_IO is new Ada.Text_IO.Enumeration_IO (Parameter_Name);
      use Parameter_IO;

      Config_File : File_Type;

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         Close (Config_File);
      end Close;

      ----------
      -- Open --
      ----------

      procedure Open  (Config_File_Name : in String) is

         procedure Read_Datas;
         --  ??

         ----------------
         -- Read_Datas --
         ----------------

         procedure Read_Datas is

            procedure Insert_Parameter (From : in String);
            --  ??

            procedure Check_Completness;
            --  ??

            Buffer : Buffer_String;
            Last   : Natural;

            -----------------------
            -- Check_Completness --
            -----------------------

            procedure Check_Completness is
            begin
               for P in Parameter_Name loop
                  if Length (Parameters (P)) = 0 then
                     raise Uncomplete_Config
                       with "Missing value for " & Parameter_Name'Image (P);

                  end if;
               end loop;
            end Check_Completness;

            ----------------------
            -- Insert_Parameter --
            ----------------------

            procedure Insert_Parameter (From : in String) is
               Last      : Natural;
               Parameter : Parameter_Name;
            begin
               Get (From, Parameter, Last);
               Parameters (Parameter) :=
                 Trim (To_Unbounded_String (From (Last + 1 .. From'Last)),
                       Side => Both);
            exception
               when Ada.IO_Exceptions.Data_Error =>
                  raise Unknown_Parameter;
            end Insert_Parameter;

         begin
            while not End_Of_File (Config_File) loop
               Get_Line (Config_File, Buffer, Last);
               if Last /= 0 then
                  Insert_Parameter (Buffer (1 .. Last));
               end if;
            end loop;
            Check_Completness;
         end Read_Datas;

      begin
         Open (Name => Config_File_Name, File => Config_File, Mode => In_File);
         Read_Datas;
      end Open;

      ----------------
      -- Save_Close --
      ----------------

      procedure Save_Close is
         Column : constant Positive_Count :=
                    Positive_Count (Parameter_Name'Width + 2);
      begin
         Reset (File => Config_File, Mode => Out_File);

         for P in Parameter_Name loop
            Put (Config_File, P);
            Set_Col (Config_File, Column);
            Put (Config_File, To_String (Parameters (P)));
            New_Line (Config_File);
         end loop;
         Close;
      end Save_Close;

   end IO;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Parameter : in Parameter_Name) return String is
   begin
      return To_String (Parameters (Parameter));
   end Get_Value;

   function Get_Value (Parameter : in Parameter_Name) return Integer is
   begin
      return Integer'Value (To_String (Parameters (Parameter)));
   end Get_Value;

   function Get_Value (Parameter : in Parameter_Name) return Float is
   begin
      return Float'Value (To_String (Parameters (Parameter)));
   end Get_Value;

   function Get_Value (Parameter : in Parameter_Name) return Boolean is
   begin
      return Boolean'Value (To_String (Parameters (Parameter)));
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Parameter : in Parameter_Name; Value : in String) is
   begin
      Parameters (Parameter) :=
        Trim (To_Unbounded_String (Value), Side => Both);
   end Set_Value;

   procedure Set_Value (Parameter : in Parameter_Name; Value : in Integer) is
   begin
      Set_Value (Parameter, Integer'Image (Value));
   end Set_Value;

   procedure Set_Value (Parameter : in Parameter_Name; Value : in Float) is
   begin
      Set_Value (Parameter, Float'Image (Value));
   end Set_Value;

   procedure Set_Value (Parameter : in Parameter_Name; Value : in Boolean) is
   begin
      Set_Value (Parameter, Boolean'Image (Value));
   end Set_Value;

   -----------------
   -- Enum_Values --
   -----------------

   package body Enum_Values is

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Parameter : in Parameter_Name) return Enum is
      begin
         return Enum'Value (To_String (Parameters (Parameter)));
      end Get_Value;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Parameter : in Parameter_Name; Value : in Enum) is
      begin
         Set_Value (Parameter, Enum'Image (Value));
      end Set_Value;

   end Enum_Values;

   -------------------
   -- Vector_Values --
   -------------------

   package body Vector_Values is

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Parameter : in Parameter_Name) return Vector is

         Param : constant String := To_String (Parameters (Parameter));

         First : Positive := 1;
         Last  : Natural  := 0;

         V     : Vector (1 .. Max_Vector_Size);
         N     : Positive := V'First;

      begin
         while Last < Param'Last loop
            Get (Param (First .. Param'Last), V (N), Last);
            N := N + 1;
            First := Last + 1;
         end loop;
         return V (1 .. N - 1);
      end Get_Value;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Parameter : in Parameter_Name; Value : in Vector) is
      begin
         Parameters (Parameter) := Null_Unbounded_String;
         for I in Value'Range loop
            Append (Parameters (Parameter),
              Trim (To_Unbounded_String (Image (Value (I))),
                Side => Both) & ' ');
         end loop;
      end Set_Value;

   end Vector_Values;

end Config;
