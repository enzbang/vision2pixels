BEGIN;

DROP TRIGGER after_post_insert;

DROP TRIGGER after_post_comment_insert;

ALTER TABLE last_user_visit RENAME TO luv_temp;

CREATE TABLE last_user_visit (
   "user_login" varchar(50),
   "post_id" integer not null,
   "last_activity" date,
   constraint unique_entry unique (user_login, post_id),
   foreign key ("post_id") references post("id"),
   foreign key ("last_activity") references comment("date")
);

INSERT INTO last_user_visit
       SELECT user_login, post_id, current_timestamp FROM luv_temp;

--  Add five seconds as we really have visited the corresponding post some
--  time after the last comment.
UPDATE last_user_visit
       SET last_activity=(SELECT datetime(comment.date, "+5 seconds")
                          FROM comment, post, luv_temp
                          WHERE luv_temp.post_id=last_user_visit.post_id
                          AND luv_temp.user_login=last_user_visit.user_login
                          AND luv_temp.last_comment_id=comment.id
                          AND last_user_visit.post_id = post.id);

DROP TABLE luv_temp;

ALTER TABLE post RENAME TO post_temp;

CREATE TABLE "post" (
   "id" integer not null primary key autoincrement,
   "name" varchar(100) not null,
   "photo_id" integer,
   "comment" longtext,
   "category_id" integer not null,
   "date_post" date default current_timestamp,
   "last_activity" date default current_timestamp,
   "template_id" integer not null,
   "visit_counter" integer not null,
   "comment_counter" integer not null,
   "hidden" boolean default FALSE,
   foreign key ("category_id") references category("id"),
   foreign key ("template_id") references template_id("id"),
   foreign key ("photo_id") references photo("id")
);

INSERT INTO post
       SELECT id, name, photo_id, comment, category_id, date_post,
              current_timestamp, template_id, visit_counter,
              comment_counter, hidden
       FROM post_temp;

UPDATE post
   SET last_activity=(SELECT COALESCE ((SELECT MAX(comment.date)
                      FROM comment, post_comment
                      WHERE post_comment.comment_id = comment.id
                            AND post_comment.post_id = post.id
                            AND comment.has_voted='FALSE'),
                      post.date_post));

DROP TABLE post_temp;

CREATE TRIGGER after_post_insert AFTER INSERT ON post
   BEGIN
      UPDATE post
         SET last_activity=datetime(current_timestamp)
         WHERE id = new.id;
      UPDATE forum
         SET last_activity=datetime(current_timestamp)
         WHERE forum.id =
               (SELECT category.forum_id
                FROM category
                WHERE category.id = new.category_id);
   END;

CREATE TRIGGER after_post_comment_insert AFTER INSERT ON post_comment
   BEGIN
      UPDATE post
         SET comment_counter=comment_counter + 1,
             last_activity=datetime(current_timestamp)
         WHERE id = (select post_id FROM comment, post_comment
                     WHERE post_comment.comment_id = new.comment_id
                     AND comment.id = post_comment.comment_id
                     AND comment.has_voted = "FALSE");
      UPDATE forum
         SET last_activity=datetime(current_timestamp)
         WHERE forum.id =
               (SELECT category.forum_id
                FROM category, post
                WHERE new.post_id = post.id
                  AND post.category_id = category.id);
      INSERT OR REPLACE INTO last_user_visit VALUES
        ((SELECT user_login FROM comment WHERE comment.id = new.comment_id),
         new.post_id,
         datetime(current_timestamp));
   END;

COMMIT;
