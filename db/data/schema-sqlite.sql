create table "user" (
   "login" varchar(50) not null primary key,
   "password" varchar(20) not null,
   "email" varchar(50) not null unique,
   "admin" boolean,
   "created" date default current_timestamp,
   "last_logged" date default current_timestamp
);

create table "user_preferences" (
   "user_login" varchar(50) not null primary key,
   "photo_per_page" integer default 10,
   "filter" varchar(15) default "SEVEN_DAYS",
   "sort" varchar(15) default "LAST_COMMENTED",
   "image_size" varchar(15) default "MAX_SIZE"
);

create table "user_to_validate" (
   "login" varchar(50) not null primary key,
   "password" varchar(20) not null,
   "email" varchar(50) not null unique,
   "created" date default current_timestamp
);

create trigger add_user_page after insert on user
   begin
      insert into user_page (user_login) values (new.login);
   end;

create table "user_page" (
   "user_login" varchar(50) not null,
   "content" longtext,
   "content_html" longtext,
   foreign key ("user_login") references user("login")
);

create table "template" (
   "id" integer not null primary key autoincrement,
   "filename" varchar(512) not null unique,
   "description" longtext not null
);

create table "comment" (
   "id" integer not null primary key autoincrement,
   "date" date default current_timestamp,
   "parent" integer null,
   "user_login" varchar(50) null,
   "anonymous_user" vachar(50) null,
   "comment" longtext not null,
   "photo_id" integer null,
   "has_voted" boolean default FALSE,
   foreign key ("photo_id") references photo("id"),
   foreign key ("user_login") references user("login"),
   foreign key ("parent") references comment("id")
);

create table "forum" (
   "id" integer not null primary key autoincrement,
   "name" varchar(100) not null,
   "last_activity" date,
   "anonymity" boolean default TRUE,
   "for_photo" boolean default TRUE
);

create table "category" (
   "id" integer not null primary key autoincrement,
   "forum_id" integer not null,
   "name" varchar(100) not null,
   foreign key ("forum_id") references post("id")
);

create table "photo" (
   "id" integer not null primary key autoincrement,
   "filename" varchar(512) unique,
   "height" integer,
   "width" integer,
   "medium_height" integer,
   "medium_width" integer,
   "thumb_height" integer,
   "thumb_width" integer,
   "size" integer
);

create trigger add_user_photo_queue after insert on user
   begin
      insert into user_photo_queue (user_login) values (new.login);
   end;

create table "user_photo_queue" (
   "user_login" varchar(50) not null,
   "photo_id" integer,
   foreign key ("user_login") references user("login"),
   foreign key ("photo_id") references photo("id")
);

create table "post" (
   "id" integer not null primary key autoincrement,
   "name" varchar(100) not null,
   "photo_id" integer,
   "comment" longtext,
   "category_id" integer not null,
   "date_post" date default current_timestamp,
   "last_comment_id" integer,
   "template_id" integer not null,
   "visit_counter" integer not null,
   "comment_counter" integer not null,
   "hidden" boolean default FALSE,
   foreign key ("category_id") references category("id"),
   foreign key ("last_comment_id") references comment("id"),
   foreign key ("template_id") references template_id("id"),
   foreign key ("photo_id") references photo("id")
);

--  We want the last_comment_id to be just one above the current
--  last_comment_id this is needed for proper ordering for lastest commented
--  photos. We want to have the new posted photos into the flow. Note that
--  this last_comment_id has meaningful value only when comment_counter is
--  not zero.

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

create table "post_comment" (
   "post_id" integer not null,
   "comment_id" integer not null unique,
   foreign key ("post_id") references post("id"),
   foreign key ("comment_id") references comment("id")
);

--  Comment counter and last_comment_id

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

create table "user_post" (
   "user_login" varchar(50) not null,
   "post_id" integer not null,
   foreign key ("post_id") references post("id"),
   foreign key ("user_login") references user("login")
);

create table "photo_metadata" (
   "photo_id" integer not null,
   "geo_latitude" real not null,
   "geo_longitude" real not null,
   "geo_latitude_formatted" varchar(20) not null,
   "geo_longitude_formatted" varchar(2) not null,
   foreign key ("photo_id") references photo("id")
);

create table "photo_exif" (
   "photo_id" integer not null,
   "create_date" varchar(19),
   "make" varchar(50),
   "camera_model_name" varchar(20),
   "shutter_speed_value" varchar(10),
   "aperture_value" varchar(10),
   "flash" varchar(20),
   "focal_length" varchar(10),
   "exposure_mode" varchar(10),
   "exposure_program" varchar(20),
   "white_balance" varchar(10),
   "metering_mode" varchar(20),
   "iso" integer,
   foreign key ("photo_id") references photo("id")
);

create table "criteria" (
   "id" integer not null primary key autoincrement,
   "name" varchar(100) not null
);

create table "rating" (
   "user_login" varchar(50) null,
   "post_id" integer not null,
   "criteria_id" integer not null,
   "post_rating" integer not null,
   foreign key ("user_login") references user("login"),
   foreign key ("post_id") references post("id"),
   foreign key ("criteria_id") references criteria("id"),
   primary key ("user_login", "post_id", "criteria_id")
);

create table "user_rating" (
   "user_login" varchar(50) not null,
   "post_id" integer not null,
   constraint unique_entry unique (user_login, post_id),
   foreign key ("user_login") references user("login"),
   foreign key ("post_id") references post("id")
);

create table global_rating (
   "post_id" integer not null,
   "criteria_id" integer not null,
   "nb_vote" integer not null,
   "post_rating_ponderated" integer not null,
   "post_rating" integer not null,
   "controversial_level" integer not null,
   foreign key ("post_id") references post("id"),
   foreign key ("criteria_id") references criteria("id"),
   primary key ("post_id", "criteria_id")
);

create trigger initialize_global_rating after insert on post
begin
   insert into global_rating
      (post_id, criteria_id, nb_vote, post_rating_ponderated,
       post_rating, controversial_level)
      select new.id, id, 0, 0, 0, 0 from criteria;
end;

create trigger update_global_rating after insert on rating
begin
   update global_rating
      set post_rating=
         (select avg(post_rating)
            from rating
            where criteria_id = new.criteria_id and post_id = new.post_id),
      nb_vote=nb_vote+1
      where criteria_id = new.criteria_id and post_id = new.post_id;
   update global_rating
      set post_rating_ponderated=post_rating*nb_vote,
          controversial_level=
             (select sum(abs(r.post_rating - g.post_rating))
                from rating r , global_rating g
                where g.criteria_id = new.criteria_id
                   and g.post_id = new.post_id
                   and r.criteria_id = new.criteria_id
                   and r.post_id = new.post_id)
      where criteria_id = new.criteria_id and post_id = new.post_id;
end;

create trigger re_update_global_rating after update on rating
begin
   update global_rating
      set post_rating=
         (select avg(post_rating)
            from rating
            where criteria_id = new.criteria_id and post_id = new.post_id)
      where criteria_id = new.criteria_id and post_id = new.post_id;
   update global_rating
      set post_rating_ponderated=post_rating*nb_vote,
          controversial_level=
             (select sum(abs(r.post_rating - g.post_rating))
                 from rating r , global_rating g
                 where g.criteria_id = new.criteria_id
                    and g.post_id = new.post_id
                    and r.criteria_id = new.criteria_id
                    and r.post_id = new.post_id)
      where criteria_id = new.criteria_id and post_id = new.post_id;
end;

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

create table photo_of_the_week (
   "id" integer not null primary key autoincrement,
   "post_id" integer,
   "val" real,
   "elected_on" date default current_date,
   foreign key ("post_id") references post("id")
);

create table user_photo_of_the_week (
   "user_login" varchar(50),
   "post_id" integer not null,
   "week_id" integer default 0,
   foreign key ("post_id") references post("id"),
   foreign key ("week_id") references photo_of_the_week("id")
);

create table vote_ponderated (
   val integer not null
);
