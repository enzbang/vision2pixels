
ALTER TABLE remember_user RENAME TO tmp_table;

CREATE TABLE remember_user (
    "user_login" varchar(50),
    "cookie_content" varchar(15),
    "last_used" date default current_timestamp
);

INSERT INTO remember_user(user_login, cookie_content)
   SELECT user_login, cookie_content FROM tmp_table;

DROP TABLE tmp_table;
