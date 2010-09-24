alter table comment add column "has_voted" boolean default FALSE;

create table "user_rating" (
   "user_login" varchar(50) not null,
   "post_id" integer not null,
   constraint unique_entry unique (user_login, post_id),
   foreign key ("user_login") references user("login"),
   foreign key ("post_id") references post("id")
);

create trigger insert_has_voted after insert on rating
begin
   insert or ignore into user_rating values (new.user_login, new.post_id);
end;

create trigger insert_has_voted_comment after insert on user_rating
begin
   insert into comment ("user_login", "comment", "has_voted")
          values (new.user_login, "vote", "TRUE");
   insert into post_comment values (new.post_id, last_insert_rowid());
end;
