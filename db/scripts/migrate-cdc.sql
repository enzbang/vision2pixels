
DROP TABLE photo_of_the_week;

CREATE TABLE photo_of_the_week (
   "id" integer not null primary key autoincrement,
   "post_id" integer,
   "val" real,
   "elected_on" date default current_date,
   foreign key ("post_id") references post("id")
);

INSERT INTO "photo_of_the_week" ('id', 'post_id', 'val') VALUES(0,0,0.0);
INSERT INTO "photo_of_the_week" VALUES(1,2,1.0, '2008-02-27');
INSERT INTO "photo_of_the_week" VALUES(2,14,3.0, '2008-03-05');
INSERT INTO "photo_of_the_week" VALUES(3,41,2.0, '2008-03-12');
INSERT INTO "photo_of_the_week" VALUES(4,42,2.0, '2008-03-19');
INSERT INTO "photo_of_the_week" VALUES(5,63,2.0, '2008-03-26');
