
create table "user_stats" (
   "user_login" varchar(50) not null primary key,
   "nb_cdc" integer default 0,
   "nb_photo" integer default 0,
   "nb_com" integer default 0,
   "nb_mess" integer default 0
);

create trigger after_photo_of_the_week_insert after insert on photo_of_the_week
   begin
      update user_stats
         set nb_cdc=nb_cdc+1
        where user_stats.user_login=
        (select user_post.user_login
        from user_post
        where user_post.post_id=new.post_id);
   end;

create trigger after_user_post_insert after insert on user_post
   begin
      update user_stats
         set nb_photo=nb_photo+1
	 where user_stats.user_login=
	    (select new.user_login
	     from post
	     where new.post_id=post.id and not post.photo_id is null);
      update user_stats
         set nb_mess=nb_mess+1
	 where user_stats.user_login=
	    (select new.user_login
	     from post
	     where new.post_id=post.id and post.photo_id is null);
   end;

create trigger after_comment_insert after insert on comment
   begin
      update user_stats
         set nb_com=nb_com+1
	 where user_stats.user_login=new.user_login
	       and new.has_voted='FALSE';
   end;

drop trigger add_user_page;

create trigger after_user_insert after insert on user
   begin
      insert into user_page (user_login) values (new.login);
      insert into user_stats (user_login) values (new.login);
   end;

--  populate the new user_stats table with current stats

insert into user_stats (user_login,nb_cdc,nb_photo,nb_com,nb_mess)
       SELECT login,
              (SELECT COUNT(potw.id)
               FROM photo_of_the_week AS potw, post, user_post AS up
               WHERE post.id=up.post_id AND post.photo_id!=0
                     AND potw.post_id=post.id
                     AND up.user_login=user.login) AS nbcdc,
              (SELECT COUNT (post_id) FROM post, user_post
               WHERE post.id=post_id AND NOT post.photo_id is null
                     AND user_post.user_login=user.login) AS nbphoto,
              (SELECT COUNT(id) FROM comment
	       WHERE user.login=comment.user_login) AS nbcom,
	      (SELECT COUNT (post_id) FROM post, user_post
               WHERE post.id=post_id AND post.photo_id is null
                     AND user_post.user_login=user.login) AS nbmess
       FROM user;
