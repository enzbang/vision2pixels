--  Modules

create table modules (
   "name" varchar(15),
   "active" boolean default TRUE
);

insert into modules values ('Thèmes', "TRUE");

--  Themes

create table themes (
   "id" integer not null primary key autoincrement,
   "title" varchar(100),
   "created" date default current_timestamp,
   "stage" integer
   --  stage: 0 open, 1 vote stage 1, 2 vote stage 2, 3 closed
);

create table themes_photos (
   "theme_id" integer,
   "photo_id" integer,
   "stage" integer default 0,
   foreign key ("photo_id") references photo("id"),
   foreign key ("theme_id") references themes("id")
);

create table themes_user_votes (
   "user_login" varchar(50),
   "photo_id" integer,
   "stage" integer,
   constraint unique_entry unique (user_login, photo_id, stage)
);

--  Update forum table

alter table forum add "active" boolean;
update forum set active='TRUE';

--  Disable current thème forum

update forum set active='FALSE' where id=6;

--  Migrate all discussion in LeBar to v2p category LeBar

insert into category values (23, 4, 'Le Bar');

update post set category_id=23 where category_id=20;

--  Close forum Le Bar

update forum set active='FALSE' where id=5;
