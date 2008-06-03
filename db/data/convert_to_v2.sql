
--  User preferences

create table "user_preferences" (
   "user_login" varchar(50) not null primary key,
   "photo_per_page" integer default 10,
   "filter" varchar(15) default "SEVEN_DAYS",
   "sort" varchar(15) default "LAST_COMMENTED",
   "image_size" varchar(15) default "MAX_SIZE"
);

--  Add date/time of forum last_activity

alter table forum add "last_activity" date;

update forum set last_activity=
  (select max(comment.date) from category, post_comment, post, comment
   where post_comment.post_id = post.id
     and post.category_id = category.id
     and category.forum_id = forum.id
     and post.last_comment_id=comment.id);

drop trigger set_last_comment_id;

create trigger after_post_insert after insert on post
   begin
      update post
         set last_comment_id=(select max(comment_id) from post_comment)
         where id = new.id;
      update forum
         set last_activity=datetime(current_timestamp)
         where forum.id =
	       (select category.forum_id
	        from category
		where category.id = new.category_id);
   end;

drop trigger update_post_status;
create trigger after_post_comment_insert after insert on post_comment
   begin
      update post
         set comment_counter=comment_counter + 1,
             last_comment_id=new.comment_id
         where id = new.post_id;
      update forum
         set last_activity=datetime(current_timestamp)
         where forum.id =
	       (select category.forum_id
	        from category, post
		where new.post_id = post.id
		  and post.category_id = category.id);
   end;

--  Allow 1000x1000 images

alter table photo add "medium_height" integer;
alter table photo add "medium_width" integer;
alter table photo add "thumb_height" integer;
alter table photo add "thumb_width" integer;
