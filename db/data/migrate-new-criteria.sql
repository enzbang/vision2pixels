-- Table
--
-- Notes:
--
-- There is 3 criteria     : Composition / Focus / Lumière
-- We want 2 criteria only : Artistique / Technique
--
-- Composition     => renamed Artistique
-- Focus + Lumière => changed to Technique
-- 
-- Copy rating table
--
-- We remove all items from rating and re-insert the proper values
-- after to activate all triggers.
--

delete from rating where user_login is null;

create table "tmp_rating" (
   "user_login" varchar(50) null,
   "post_id" integer not null,
   "criteria_id" integer not null,
   "post_rating" integer not null,
   foreign key ("user_login") references user("login"),
   foreign key ("post_id") references post("id"),
   foreign key ("criteria_id") references criteria("id"),
   primary key ("user_login", "post_id", "criteria_id")
);

--  Add items for Composition as-is

insert into tmp_rating
       select user_login, post_id, criteria_id, post_rating
       from rating as r
       where r.criteria_id = 1;

--  Add items for other criteria (average of all notes)

insert into tmp_rating 
       select user_login, post_id, criteria_id, (select round(avg(post_rating)) from rating as ir where ir.post_id = r.post_id and ir.criteria_id != 1 and ir.user_login = r.user_login)
       from rating as r
       where r.criteria_id = 2;

--  Drop rating table

delete from rating;

delete from global_rating where criteria_id=3;
update global_rating
       set nb_vote=0, post_rating_ponderated=0, post_rating=0, controversial_level=0;

--  Now insert back the votes

insert into rating
       select user_login, post_id, criteria_id, post_rating
       from tmp_rating;

--  Drop tmp_rating

drop table tmp_rating;

--  Last, rename the rating

update criteria set name="Artistique" where id=1;
update criteria set name="Technique" where id=2;
