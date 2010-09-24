alter table user_to_validate add "nb_reminder" integer default 0;
alter table user_to_validate add "last_reminder" date;

update user_to_validate set last_reminder=created;
