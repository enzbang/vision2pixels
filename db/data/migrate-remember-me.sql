create table remember_user (
    "user_login" varchar(50),
    "cookie_content" varchar(15)
);

alter table user add "remember" boolean;

