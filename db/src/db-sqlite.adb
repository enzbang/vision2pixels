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
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body DB.SQLite is

   use GNU.DB;

   procedure Unchecked_Free is
     new Unchecked_Deallocation (SQLite3.Object, SQLite3.Handle);

   procedure Check_Result
     (Routine : in String;
      Result  : in SQLite3.Return_Value);
   pragma Inline (Check_Result);
   --  Check result, raises and exception if it is an error code

   procedure Step_Internal (Iter : in out Iterator);
   --  Advance to the next row and set Iter.More

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (DB : in Handle) is
   begin
      Execute (DB, "begin");
   end Begin_Transaction;

   ------------------
   -- Check_Result --
   ------------------

   procedure Check_Result
     (Routine : in String;
      Result  : in SQLite3.Return_Value)
   is
      use type SQLite3.Return_Value;
   begin
      if Result /= SQLite3.SQLITE_OK then
         raise DB_Error
           with "SQLite: Error " & SQLite3.Return_Value'Image (Result) &
             " in " & Routine;
      end if;
   end Check_Result;

   -----------
   -- Close --
   -----------

   procedure Close (DB : in out Handle) is
   begin
      Check_Result ("close", SQLite3.Close (DB.H));
      Unchecked_Free (DB.H);
   end Close;

   ------------
   -- Commit --
   ------------

   procedure Commit (DB : in Handle) is
   begin
      Execute (DB, "commit");
   end Commit;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (DB       : in out Handle;
      Name     : in String;
      User     : in String := "";
      Password : in String := "")
   is
      pragma Unreferenced (User, Password);
      use type GNU.DB.SQLite3.Handle;
   begin
      if DB.H = null then
         DB.H := new GNU.DB.SQLite3.Object;
      end if;
      Check_Result ("connect", SQLite3.Open (DB.H, Name));
   end Connect;

   ----------------
   -- End_Select --
   ----------------

   procedure End_Select (Iter : in out Iterator) is
   begin
      Check_Result ("end_select", SQLite3.finalize (Iter.S'Unchecked_Access));
   end End_Select;

   -------------
   -- Execute --
   -------------

   procedure Execute (DB : in Handle; SQL : in String) is
   begin
      Check_Result ("execute", SQLite3.Exec (DB.H, SQL));
   end Execute;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Iter   : in out Iterator;
      Result :    out String_Vectors.Vector)
   is
      use type SQLite3.Return_Value;
   begin
      for K in 0 .. Iter.Col - 1 loop
         String_Vectors.Append
           (Result, SQLite3.column_text (Iter.S'Unchecked_Access, K));
      end loop;

      Step_Internal (Iter);
   end Get_Line;

   ----------
   -- More --
   ----------

   function More (Iter : in Iterator) return Boolean is
   begin
      return Iter.More;
   end More;

   --------------------
   -- Prepare_Select --
   --------------------

   procedure Prepare_Select
     (DB   : in     Handle;
      Iter : in out Standard.DB.Iterator'Class;
      SQL  : in     String)
   is
      use type SQLite3.Statement_Reference;
   begin
      pragma Assert (Iter in Iterator);

      Iterator (Iter).H := DB;
      Iterator (Iter).More := True;

      Check_Result
        ("prepare_select",
         SQLite3.prepare (DB.H, SQL, Iterator (Iter).S'Unchecked_Access));

      Iterator (Iter).Col :=
        SQLite3.column_count (Iterator (Iter).S'Unchecked_Access);

      Step_Internal (Iterator (Iter));
   end Prepare_Select;

   --------------
   -- Rollback --
   --------------

   procedure Rollback (DB : in Handle) is
   begin
      Execute (DB, "rollback");
   end Rollback;

   -------------------
   -- Step_Internal --
   -------------------

   procedure Step_Internal (Iter : in out Iterator) is
      use type SQLite3.Return_Value;
      R : SQLite3.Return_Value;
   begin
      R := SQLite3.step (Iter.S'Unchecked_Access);

      if R = SQLite3.SQLITE_DONE then
         Iter.More := False;
      elsif R = SQLite3.SQLITE_ROW then
         Iter.More := True;
      else
         Check_Result ("step_internal", R);
         Iter.More := False;
      end if;
   end Step_Internal;

end DB.SQLite;
