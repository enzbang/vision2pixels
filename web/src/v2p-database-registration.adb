------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                         Copyright (C) 2009-2010                          --
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

with AWS.Utils;

with Morzhol.Strings;

with V2P.DB_Handle;
with V2P.Email;
with V2P.User_Validation;

with V2P.Template_Defs.Block_Users_To_Validate;
with V2P.Template_Defs.R_Block_Users_To_Validate;

private with V2P.Database.Support;

package body V2P.Database.Registration is

   use Morzhol.Strings;
   use V2P.Database.Support;

   Lock_Register : Utils.Semaphore;
   --  Lock the application when registering a new user. We want to avoid two
   --  users registering under the same login.

   -----------------
   -- Delete_User --
   -----------------

   function Delete_User (Login : in String) return Boolean is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      DBH.Handle.Execute
        ("DELETE FROM user_to_validate WHERE login=" & Q (Login));
      return True;
   end Delete_User;

   function Delete_User (Login, Key : in String) return Boolean is
      DBH             : constant TLS_DBH_Access :=
                          TLS_DBH_Access (DBH_TLS.Reference);
      Iter            : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line            : DB.String_Vectors.Vector;
      Password, Email : Unbounded_String;
   begin
      Connect (DBH);

      --  Read user's data from user_to_validate

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT password, email FROM user_to_validate "
         & "WHERE login=" & Q (Login));

      if Iter.More then
         Iter.Get_Line (Line);
         Password := +DB.String_Vectors.Element (Line, 1);
         Email    := +DB.String_Vectors.Element (Line, 2);
         Line.Clear;

      else
         --  User not found, could be due to an obsolete registration URL sent
         return False;
      end if;

      Iter.End_Select;

      --  Check key now

      if User_Validation.Key (Login, -Password, -Email) /= Key then
         --  Key does not match
         return False;
      end if;

      --  Now we can remove the user from user_to_validate

      return Delete_User (Login);
   end Delete_User;

   -------------------------------
   -- Get_User_To_Validate_Data --
   -------------------------------

   function Get_User_To_Validate_Data (Uid : in String) return User_Data is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      if Uid = "" then
         return No_User_Data;
      end if;

      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT password, email " &
         "FROM user_to_validate WHERE login=" & Q (Uid));

      if Iter.More then
         Iter.Get_Line (Line);

         return User_Data'
           (UID         => +Uid,
            Password    => +DB.String_Vectors.Element (Line, 1),
            Admin       => False,
            Email       => +DB.String_Vectors.Element (Line, 2),
            Preferences => Default_User_Settings);
      else
         return No_User_Data;
      end if;
   end Get_User_To_Validate_Data;

   -----------------------------
   -- Register_New_User_Email --
   -----------------------------

   procedure Register_New_User_Email
     (Uid : in String; New_Email : in String)
   is
      SQL : constant String :=
              "UPDATE user SET new_email=" & Q (New_Email)
              & " WHERE login=" & Q (Uid);
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   end Register_New_User_Email;

   -------------------
   -- Register_User --
   -------------------

   function Register_User
     (Login, Password, Email : in String) return Boolean
   is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.Iterator'Class := DB_Handle.Get_Iterator;
   begin
      Lock_Register.Seize;

      Connect (DBH);

      --  Check already validated users

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT * FROM user "
         & "WHERE login COLLATE NOCASE =" & Q (Login)
         & " OR email COLLATE NOCASE =" & Q (Email));

      if Iter.More then
         Iter.End_Select;
         Lock_Register.Release;
         return False;
      end if;

      --  Check registered but not yet validated users

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT * FROM user_to_validate "
         & "WHERE login=" & Q (Login) & " OR email=" & Q (Email));

      if Iter.More then
         Iter.End_Select;
         Lock_Register.Release;
         return False;
      end if;

      --  The login and e-mail are free, register user

      DBH.Handle.Execute
        ("INSERT INTO user_to_validate ('login', 'password', 'email') "
         & "VALUES ("
         & Q (Login) & ", " & Q (Password) & ", " & Q (Email) & ')');

      Lock_Register.Release;
      return True;

   exception
      when others =>
         Lock_Register.Release;
         return False;
   end Register_User;

   -------------------
   -- Send_Reminder --
   -------------------

   function Send_Reminder
     (Login : in String := "") return Templates.Translate_Set
   is
      use type Templates.Tag;
      SQL    : constant String :=
                 "SELECT login, password, email FROM user_to_validate";
      DBH    : constant TLS_DBH_Access :=
                 TLS_DBH_Access (DBH_TLS.Reference);
      Iter   : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line   : DB.String_Vectors.Vector;
      Error  : Unbounded_String;
      Result : Templates.Translate_Set;
   begin
      Connect (DBH);

      if Login = "" then
         DBH.Handle.Prepare_Select (Iter, SQL);
      else
         DBH.Handle.Prepare_Select (Iter, SQL & " WHERE login=" & Q (Login));
      end if;

      while Iter.More loop
         Iter.Get_Line (Line);

         begin
            Email.Send_Reminder
              (Login    => DB.String_Vectors.Element (Line, 1),
               Password => DB.String_Vectors.Element (Line, 2),
               Email    => DB.String_Vectors.Element (Line, 3));

            DBH.Handle.Execute
              ("UPDATE user_to_validate"
               & " SET nb_reminder=nb_reminder+1, "
               & "     last_reminder=DATETIME('NOW')"
               & " WHERE login=" & Q (DB.String_Vectors.Element (Line, 1)));
         exception
            when others =>
               Append (Error, DB.String_Vectors.Element (Line, 1) & " ");
         end;
      end loop;

      Line.Clear;

      Templates.Insert
        (Result,
         Templates.Assoc
           (Template_Defs.R_Block_Users_To_Validate.ERROR,
            Error /= Null_Unbounded_String));

      return Result;
   end Send_Reminder;

   -----------------------
   -- Users_To_Validate --
   -----------------------

   function Users_To_Validate return Templates.Translate_Set is
      use type Templates.Tag;
      SQL           : constant String :=
                        "SELECT login, email, created, nb_reminder,"
                          & " last_reminder FROM user_to_validate";
      DBH           : constant TLS_DBH_Access :=
                        TLS_DBH_Access (DBH_TLS.Reference);
      Iter          : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line          : DB.String_Vectors.Vector;
      Login         : Templates.Tag;
      Email         : Templates.Tag;
      Created       : Templates.Tag;
      Nb_Reminder   : Templates.Tag;
      Last_Reminder : Templates.Tag;
      Set           : Templates.Translate_Set;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select (Iter, SQL);

      while Iter.More loop
         Iter.Get_Line (Line);

         Login         := Login & DB.String_Vectors.Element (Line, 1);
         Email         := Email & DB.String_Vectors.Element (Line, 2);
         Created       := Created & DB.String_Vectors.Element (Line, 3);
         Nb_Reminder   := Nb_Reminder & DB.String_Vectors.Element (Line, 4);
         Last_Reminder := Last_Reminder & DB.String_Vectors.Element (Line, 5);
      end loop;

      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Users_To_Validate.LOGIN, Login));
      Templates.Insert
        (Set,
         Templates.Assoc (Template_Defs.Block_Users_To_Validate.EMAIL, Email));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_Users_To_Validate.CREATED, Created));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_Users_To_Validate.NB_REMINDER, Nb_Reminder));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Block_Users_To_Validate.LAST_REMINDER,
            Last_Reminder));

      Iter.End_Select;
      return Set;
   end Users_To_Validate;

   -----------------------------
   -- Validate_New_User_Email --
   -----------------------------

   function Validate_New_User_Email (Uid, Key : in String) return Boolean is
      DBH   : constant TLS_DBH_Access :=
                 TLS_DBH_Access (DBH_TLS.Reference);
      Iter  : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line  : DB.String_Vectors.Vector;
      SQL   : constant String :=
                "UPDATE user SET email=new_email WHERE login=" & Q (Uid);
      Email : Unbounded_String;
   begin
      Connect (DBH);

      --  Read user's data from user_to_validate

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT new_email FROM user WHERE login=" & Q (Uid));

      if Iter.More then
         Iter.Get_Line (Line);
         Email    := +DB.String_Vectors.Element (Line, 1);
         Line.Clear;

      else
         --  User not found, could be due to an obsolete registration URL sent
         return False;
      end if;

      Iter.End_Select;

      if User_Validation.Key (Uid, "", -Email) /= Key then
         --  Key does not match
         return False;
      end if;

      DBH.Handle.Execute (SQL);

      return True;
   end Validate_New_User_Email;

   -------------------
   -- Validate_User --
   -------------------

   function Validate_User (Login, Key : in String) return Boolean is
      DBH             : constant TLS_DBH_Access :=
                          TLS_DBH_Access (DBH_TLS.Reference);
      Iter            : DB.Iterator'Class := DB_Handle.Get_Iterator;
      Line            : DB.String_Vectors.Vector;
      Password, Email : Unbounded_String;
   begin
      Connect (DBH);

      --  Read user's data from user_to_validate

      DBH.Handle.Prepare_Select
        (Iter,
         "SELECT password, email FROM user_to_validate "
         & "WHERE login=" & Q (Login));

      if Iter.More then
         Iter.Get_Line (Line);
         Password := +DB.String_Vectors.Element (Line, 1);
         Email    := +DB.String_Vectors.Element (Line, 2);
         Line.Clear;

      else
         --  User not found, could be due to an obsolete registration URL sent
         return False;
      end if;

      Iter.End_Select;

      --  Check key now

      if User_Validation.Key (Login, -Password, -Email) /= Key then
         --  Key does not match
         return False;
      end if;

      --  Create corresponding entry into user table

      DBH.Handle.Execute
        ("INSERT INTO user ('login', 'password', 'email', 'admin') VALUES ("
         & Q (Login) & ", " & Q (-Password)
         & ", " & Q (-Email) & ", 'FALSE')");

      --  Now we can remove the user from user_to_validate

      DBH.Handle.Execute
        ("DELETE FROM user_to_validate WHERE login=" & Q (Login));

      return True;
   end Validate_User;

end V2P.Database.Registration;
