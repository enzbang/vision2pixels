BEGIN;
ALTER TABLE post RENAME TO post_temp;
create table "post" (
   "id" integer not null primary key autoincrement,
   "name" varchar(100) not null,
   "photo_id" integer,
   "comment" longtext,
   "category_id" integer not null,
   "date_post" date default current_timestamp,
   "last_comment_id" integer default 0,
   "template_id" integer not null,
   "visit_counter" integer not null,
   "comment_counter" integer not null,
   "hidden" boolean default FALSE,
   foreign key ("category_id") references category("id"),
   foreign key ("last_comment_id") references comment("id"),
   foreign key ("template_id") references template_id("id"),
   foreign key ("photo_id") references photo("id")
);
INSERT INTO post SELECT * FROM post_temp;
DROP TABLE post_temp;

create table last_user_visit (
   "user_login" varchar(50),
   "post_id" integer not null,
   "last_comment_id" integer,
   constraint unique_entry unique (user_login, post_id),
   foreign key ("post_id") references post("id"),
   foreign key ("last_comment_id") references comment("id")
);

drop trigger after_post_comment_insert;

create trigger after_post_comment_insert after insert on post_comment
   begin
      update post
         set comment_counter=comment_counter + 1,
             last_comment_id=new.comment_id
         where id = (select post_id from comment, post_comment
                     where post_comment.comment_id = new.comment_id
                     and comment.id = post_comment.comment_id
                     and comment.has_voted = "FALSE");
      update forum
         set last_activity=datetime(current_timestamp)
         where forum.id =
	       (select category.forum_id
	        from category, post
		where new.post_id = post.id
		  and post.category_id = category.id);
      insert or replace into last_user_visit values
        ((select user_login from comment where comment.id = new.comment_id),
         new.post_id,
         new.comment_id);
   end;

COMMIT;
