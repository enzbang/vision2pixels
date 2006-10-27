--  Create users

insert into user values ('enzbang', 'password', 'v2p@ramonat.fr');
insert into user values ('turbo', 'turbopass', 'v2p@obry.net');


--  Create category

insert into category values (1, 'Test');

--  Create template

insert into template values (1, 'mytemplate', 'V2P Template');

--  Create photo

insert into photo
 ('name', 'filename', 'category_id',
  'date_publication', 'template_id',
  'visit_counter', 'comment_counter')
  values
  ('une première photo', 'toto.jpg', 1, 'NOW',
   1, 0, 0);

insert into user_photo values
 ('enzbang', 1);

--  Photo comments

insert into comment
  ('date', 'user_login', 'comment')
  values
  ('NOW', 'turbo', 'What a beautiful landscape !');

insert into photo_comment values (1, 1);

insert into comment
  ('date', 'parent', 'user_login', 'comment')
  values
  ('NOW', 1, 'enzbang', 'What ? This is a portrait !');

insert into photo_comment values (1, 2);

insert into comment
  ('date', 'parent', 'user_login', 'comment')
  values
  ('NOW', 2, 'turbo', 'Oups sorry ;)');

insert into photo_comment values (1, 3);

insert into comment
  ('date', 'user_login', 'comment')
  values
  ('NOW', 'turbo', 'Why not another thread ?');

insert into photo_comment values (1, 4);

insert into comment
  ('date', 'parent', 'user_login', 'comment')
  values
  ('NOW', 4, 'enzbang', "Oh dear, it's wonderful");

insert into photo_comment values (1, 5);

insert into comment
  ('date', 'parent', 'user_login', 'comment')
  values
  ('NOW', 5, 'turbo', 'Yes, it is.');

insert into photo_comment values (1, 6);







