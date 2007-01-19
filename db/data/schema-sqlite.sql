create table "user" (
"login" varchar(50) not null primary key,
"password" varchar(20) not null,
"email" varchar(50) not null unique
);

create table "template" (
"id" integer not null primary key autoincrement,
"filename" varchar(512) not null unique,
"description" longtext not null
);

create table "comment" (
"id" integer not null primary key autoincrement,
"date" date default current_timestamp,
"parent" integer null ,
"user_login" varchar(50) null ,
"anonymous_user" vachar(50) null,
"comment" longtext not null ,
"filename" varchar(512) null ,
foreign key ("user_login") references user("login"),
foreign key ("parent") references comment("id")
);

create table "forum" (
"id" integer not null primary key autoincrement,
"name" varchar(100) not null
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
"size" integer
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
foreign key ("category_id") references category("id"),
foreign key ("last_comment_id") references comment("id"),
foreign key ("template_id") references template_id("id"),
foreign key ("photo_id") references photo("id")
);

create table "post_comment" (
"post_id" integer not null,
"comment_id" integer not null unique,
foreign key ("post_id") references post("id"),
foreign key ("comment_id") references comment("id")
);

create table "user_post" (
"user_login" varchar(50) not null,
"post_id" integer not null,
foreign key ("post_id") references post("id"),
foreign key ("user_login") references user("login")
);
