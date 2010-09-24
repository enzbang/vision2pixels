create temp table vote_photo_of_the_week (
   "user_login" varchar(50) not null,
   "nb_vote" integer
);

insert into vote_photo_of_the_week (user_login, nb_vote)
       select user_login, count(*)
              from user_photo_of_the_week
              where week_id = 0
              group by user_login;

create temp table score_photo_of_the_week (
   "post_id" integer not null,
   "user_login" varchar(50) not null,
   "val" integer,
   foreign key ("post_id") references post("id")
);

insert into score_photo_of_the_week (post_id, user_login, val)
       select post_id, u.user_login, p.val
              from user_photo_of_the_week u, vote_photo_of_the_week v, vote_ponderated p
              where week_id = 0 and u.user_login = v.user_login and p.rowid = v.nb_vote;

insert into photo_of_the_week (val, post_id)
       select sum(val) as total, post_id
              from score_photo_of_the_week
              group by (post_id) order by total desc limit 1;

update user_photo_of_the_week
       set week_id = (select max(id) from photo_of_the_week)
       where week_id = 0;
