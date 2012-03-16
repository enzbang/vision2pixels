------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2012                            --
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

with V2P.DB_Handle;

with V2P.Template_Defs.Block_Modules_List;

package body V2P.Database.Modules is

   --------------
   -- Get_List --
   --------------

   function Get_List return Templates.Translate_Set is
      use type Templates.Tag;

      DBH   : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL   : constant String :=
                "SELECT name FROM modules WHERE active='TRUE'";
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      Name  : Templates.Tag;
      Set   : Templates.Translate_Set;
   begin
      Connect (DBH);
      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);
         Name  := Name  & DB.String_Vectors.Element (Line, 1);
         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Modules_List.MODULE_NAME, Name));
      return Set;
   end Get_List;

end V2P.Database.Modules;
