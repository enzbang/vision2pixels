------------------------------------------------------------------------------
--                              Vision2Pixels                               --
--                                                                          --
--                            Copyright (C) 2009                            --
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

with Ada.Exceptions; use Ada.Exceptions;

with AWS.Attachments;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.SMTP.Client;
with AWS.Templates;

with V2P.Settings;
with V2P.User_Validation;

with V2P.Template_Defs.Email_Send_Reminder;

package body V2P.Email is

   use AWS;

   procedure Send
     (Login, To    : in String;
      Template     : in String;
      Translations : in Templates.Translate_Set;
      Subject      : in String);
   --  Send an e-mail message given the subject and template with corresponding
   --  data.

   ----------
   -- Send --
   ----------

   procedure Send
     (Login, To    : in String;
      Template     : in String;
      Translations : in Templates.Translate_Set;
      Subject      : in String)
   is
      SMTP_Server : constant SMTP.Receiver :=
                      SMTP.Client.Initialize (Settings.SMTP_Server);
      Content     : Attachments.List;
      Headers     : AWS.Headers.List;
      Result      : SMTP.Status;
   begin
      AWS.Headers.Set.Add
        (Headers,
         Messages.Content_Type_Token,
         MIME.Text_Plain & "; charset=UTF-8");

      Attachments.Add
        (Content,
         Name    => "message",
         Data    => Attachments.Value
           (Data   => Templates.Parse (Template, Translations),
            Encode => Attachments.Base64),
         Headers => Headers);

      SMTP.Client.Send
        (Server      => SMTP_Server,
         From        => SMTP.E_Mail ("V2P", "no-reply@no-reply.com"),
         To          => SMTP.Recipients'(1 => SMTP.E_Mail (Login, To)),
         Subject     => Subject,
         Attachments => Content,
         Status      => Result);

      if not SMTP.Is_Ok (Result) then
         raise Cannot_Send
           with "(Email.Send) : sending e-mail failed for email "
             & To & " with template " & Template;
      end if;

   exception
      when E : others =>
         raise Cannot_Send with Exception_Message (E);
   end Send;

   -------------------
   -- Send_Reminder --
   -------------------

   procedure Send_Reminder (Login, Password, Email : in String) is
      Key : constant String :=
              User_Validation.Key (Login, Password, Email);
      Set : Templates.Translate_Set;
   begin
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Email_Send_Reminder.USER_LOGIN, Login));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Email_Send_Reminder.USER_EMAIL, Email));
      Templates.Insert
        (Set,
         Templates.Assoc
           (Template_Defs.Email_Send_Reminder.KEY, Key));

      Send
        (Login        => Login,
         To           => Email,
         Template     => Template_Defs.Email_Send_Reminder.Template,
         Translations => Set,
         Subject      => "Rappel d'inscription Vision2Pixels");
   end Send_Reminder;

end V2P.Email;
