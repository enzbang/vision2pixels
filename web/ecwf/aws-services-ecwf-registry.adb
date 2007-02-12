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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with AWS.Parameters;
with AWS.Status.Set;

package body AWS.Services.ECWF.Registry is

   use Ada;

   Internal_Context_Var : constant String := "=&= CTX_ECWF =&=";
   Context_Var          : constant String := "CTX_ECWF";

   type Lazy_Handler is new Templates.Dynamic.Lazy_Tag with record
      Request      : Status.Data;
      --  Current request made to the server
      Translations : Templates.Translate_Set;
      --  Global translations table
   end record;

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : in              String;
      Translations : in out          Templates.Translate_Set);

   type Web_Object is record
      Content_Type : Unbounded_String;
      Template     : Unbounded_String;
      Data_CB      : access procedure
        (Request      : in Status.Data;
         Context      : access ECWF.Context.Object;
         Translations : in out Templates.Translate_Set);
   end record;

   package Web_Object_Maps is new Containers.Indefinite_Hashed_Maps
     (String, Web_Object, Strings.Hash, "=");
   use Web_Object_Maps;

   WO_Map : Map;

   function Get_Context_Id
     (Lazy_Tag : not null access Lazy_Handler'Class) return Context.Id;
   --  Get the proper context id for this request

   -----------
   -- Build --
   -----------

   function Build
     (Key           : in String;
      Request       : in Status.Data;
      Translations  : in Templates.Translate_Set;
      Status_Code   : in Messages.Status_Code := Messages.S200;
      Cache_Control : in Messages.Cache_Option := Messages.Unspecified)
      return Response.Data
   is
      P : constant Page := Parse (Key, Request, Translations);
   begin
      if P = No_Page then
         return Response.Build
           (MIME.Text_HTML, "", Status_Code => Messages.S404);

      else
         return Response.Build
           (To_String (P.Content_Type),
            To_String (P.Content),
            Status_Code   => Status_Code,
            Cache_Control => Cache_Control);
      end if;
   end Build;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id
     (Lazy_Tag : not null access Lazy_Handler'Class) return Context.Id
   is
      use type Context.Id;

      function Create_New_Context return Context.Id;
      --  Create a new context, register it and return the corresponding Id

      ------------------------
      -- Create_New_Context --
      ------------------------

      function Create_New_Context return Context.Id is
         C : constant Context.Id := Context.Create;
      begin
         Status.Set.Add_Parameter
           (Lazy_Tag.Request, Internal_Context_Var, Context.Image (C));
         return C;
      end Create_New_Context;

   begin
      if Parameters.Get
        (Status.Parameters (Lazy_Tag.Request), Internal_Context_Var) = ""
      then
         --  No context known

         if Parameters.Get
           (Status.Parameters (Lazy_Tag.Request), Context_Var) = ""
         then
            --  No context sent with the request, create a new context for
            --  this request.

            return Create_New_Context;

         else
            --  A context has been sent with this request

            declare
               C_Str : constant String :=
                         Parameters.Get
                           (Status.Parameters
                              (Lazy_Tag.Request), Context_Var);
               CID   : constant Context.Id := Context.Value (C_Str);
            begin
               --  First check that it is a know context (i.e. still a valid
               --  context recorded in the context database).

               if Context.Exist (CID) then
                  --  This context is known, record it as the current
                  --  working context.

                  Status.Set.Add_Parameter
                    (Lazy_Tag.Request, Internal_Context_Var, C_Str);
                  return CID;

               else
                  --  Unknown or expired context, create a new one

                  return Create_New_Context;
               end if;
            end;
         end if;

      else
         --  Context already recorded, just retrieve it
         return Context.Value
           (Parameters.Get
              (Status.Parameters (Lazy_Tag.Request), Internal_Context_Var));
      end if;
   end Get_Context_Id;

   -----------
   -- Parse --
   -----------

   function Parse
     (Key          : in String;
      Request      : in Status.Data;
      Translations : in Templates.Translate_Set) return Page
   is
      LT       : aliased Lazy_Handler :=
                   (Templates.Dynamic.Lazy_Tag with Request, Translations);
      Position : Web_Object_Maps.Cursor;
   begin
      --  Get Web Object

      Position := WO_Map.Find (Key);

      if Position = No_Element then
         return No_Page;

      else
         declare
            Context : aliased ECWF.Context.Object :=
                        ECWF.Context.Get (Get_Context_Id (LT'Access));
            T       : Templates.Translate_Set;
         begin
            --  Get translation set for this tag

            if Element (Position).Data_CB /= null then
               Element (Position).Data_CB (LT.Request, Context'Access, T);
            end if;

            Templates.Insert (T, Translations);

            return (Content_Type => Element (Position).Content_Type,
                    Content      => Templates.Parse
                      (To_String (Element (Position).Template), T,
                       Lazy_Tag => LT'Unchecked_Access),
                    Set          => Templates.Null_Set);
         end;
      end if;
   end Parse;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key          : in String;
      Template     : in String;
      Data_CB      : not null access procedure
        (Request      : in Status.Data;
         Context      : access ECWF.Context.Object;
         Translations : in out Templates.Translate_Set);
      Content_Type : in String := MIME.Text_HTML)
   is
      WO : Web_Object;
   begin
      --  WO := (To_Unbounded_String (Template), Data_CB);
      --  ??? problem with GNAT GPL 2006.

      WO.Content_Type := To_Unbounded_String (Content_Type);
      WO.Template     := To_Unbounded_String (Template);
      WO.Data_CB      := Data_CB;

      --  Register Tag

      WO_Map.Include (Key, WO);
   end Register;

   -----------
   -- Value --
   -----------

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : in              String;
      Translations : in out          Templates.Translate_Set)
   is
      Position : Web_Object_Maps.Cursor;
   begin
      --  Specific case for the contextual var

      if Var_Name = Context_Var then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Context_Var,  Context.Image (Get_Context_Id (Lazy_Tag))));
         return;
      end if;

      --  Get Web Object

      Position := WO_Map.Find (Var_Name);

      if Position /= No_Element then
         declare
            Context : aliased ECWF.Context.Object :=
                        ECWF.Context.Get (Get_Context_Id (Lazy_Tag));
            T       : Templates.Translate_Set;
         begin
            --  Get translation set for this tag

            if Element (Position).Data_CB /= null then
               Element (Position).Data_CB
                 (Lazy_Tag.Request, Context'Access, T);
            end if;

            Templates.Insert (T, Translations);
            Templates.Insert (T, Lazy_Tag.Translations);

            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Var_Name,
                  Unbounded_String'(Templates.Parse
                    (To_String (Element (Position).Template), T,
                       Lazy_Tag =>
                         Templates.Dynamic.Lazy_Tag_Access (Lazy_Tag)))));
         end;
      end if;
   end Value;

end AWS.Services.ECWF.Registry;
