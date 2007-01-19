--  Create users

insert into user values ('enzbang', 'password', 'v2p@ramonat.fr');
insert into user values ('turbo', 'turbopass', 'v2p@obry.net');

--  Create forums

insert into forum values (1, "Forum photographies");
insert into forum values (2, "Forum matériel");

--  Create categories

insert into category values (1, 1, 'Test');
insert into category values (2, 2, 'MatCat1');
insert into category values (3, 2, 'MatCat2');

--  Create templates

insert into template values (1, 'mytemplate', 'V2P Template');

--  Create post

insert into post
   ('name', 'category_id',
    'template_id', 'visit_counter', 'comment_counter')
  values ('un premier post', 1, 1, 0, 0);

insert into user_post values ('enzbang', 1);

--  Comment counter

create trigger update_comment_counter insert on comment
   begin
      update post set comment_counter=comment_counter + 1;
   end;

--  Post comments

insert into comment ('user_login', 'comment')
  values ('turbo', 'What a beautiful landscape !');

insert into post_comment values (1, 1);

insert into comment ('parent', 'user_login', 'comment')
  values (1, 'enzbang', 'What ? This is a portrait !');

insert into post_comment values (1, 2);

insert into comment ('parent', 'user_login', 'comment')
  values (2, 'turbo', 'Oups sorry ;)');

insert into post_comment values (1, 3);

insert into comment ('user_login', 'comment')
  values ('turbo', 'Why not another thread ?');

insert into post_comment values (1, 4);

insert into comment ('parent', 'user_login', 'comment')
  values (4, 'enzbang', "Oh dear, it's wonderful");

insert into post_comment values (1, 5);

insert into comment ('parent', 'user_login', 'comment')
  values (5, 'turbo', 'Yes, it is.');

insert into post_comment values (1, 6);


-- Many post information for testing

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Cameras', 1, 1, 0, 0, "2007-01-01 00:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('A post', 1, 1, 0, 0, "2007-01-18 12:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Another Post', 1, 1, 0, 0, "2007-01-17 00:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post', 1, 1, 0, 0, "2007-01-16 00:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 1', 1, 1, 0, 0, "2007-01-16 05:00:31");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 1', 1, 1, 0, 0, "2007-01-16 01:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 2', 1, 1, 0, 0, "2007-01-16 02:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 3', 1, 1, 0, 0, "2007-01-16 03:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 4', 1, 1, 0, 0, "2007-01-16 04:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 5', 1, 1, 0, 0, "2007-01-16 05:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 6', 1, 1, 0, 0, "2007-01-16 06:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 7', 1, 1, 0, 0, "2007-01-16 07:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 8', 1, 1, 0, 0, "2007-01-16 08:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 9', 1, 1, 0, 0, "2007-01-16 09:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 10', 1, 1, 0, 0, "2007-01-16 10:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 11', 1, 1, 0, 0, "2007-01-16 11:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 12', 1, 1, 0, 0, "2007-01-16 12:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 13', 1, 1, 0, 0, "2007-01-16 13:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 14', 1, 1, 0, 0, "2007-01-16 14:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 15', 1, 1, 0, 0, "2007-01-16 15:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 16', 1, 1, 0, 0, "2007-01-16 16:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 17', 1, 1, 0, 0, "2007-01-16 17:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 18', 1, 1, 0, 0, "2007-01-16 18:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 19', 1, 1, 0, 0, "2007-01-16 19:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 20', 1, 1, 0, 0, "2007-01-16 20:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 21', 1, 1, 0, 0, "2007-01-16 21:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 22', 1, 1, 0, 0, "2007-01-16 22:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 23', 1, 1, 0, 0, "2007-01-16 23:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post 24', 1, 1, 0, 0, "2007-01-16 24:00:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 1', 1, 1, 0, 0, "2007-01-17 10:01:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 2', 1, 1, 0, 0, "2007-01-17 10:02:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 3', 1, 1, 0, 0, "2007-01-17 10:03:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 4', 1, 1, 0, 0, "2007-01-17 10:04:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 5', 1, 1, 0, 0, "2007-01-17 10:05:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 6', 1, 1, 0, 0, "2007-01-17 10:06:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 7', 1, 1, 0, 0, "2007-01-17 10:07:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 8', 1, 1, 0, 0, "2007-01-17 10:08:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 9', 1, 1, 0, 0, "2007-01-17 10:09:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 10', 1, 1, 0, 0, "2007-01-17 11:10:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 11', 1, 1, 0, 0, "2007-01-17 11:11:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 12', 1, 1, 0, 0, "2007-01-17 11:12:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 13', 1, 1, 0, 0, "2007-01-17 11:13:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 14', 1, 1, 0, 0, "2007-01-17 11:14:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 15', 1, 1, 0, 0, "2007-01-17 11:15:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 16', 1, 1, 0, 0, "2007-01-17 11:16:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 17', 1, 1, 0, 0, "2007-01-17 11:17:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 18', 1, 1, 0, 0, "2007-01-17 11:18:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 19', 1, 1, 0, 0, "2007-01-17 11:19:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 20', 1, 1, 0, 0, "2007-01-17 11:20:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 21', 1, 1, 0, 0, "2007-01-17 11:21:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 22', 1, 1, 0, 0, "2007-01-17 11:22:01");

insert into post ('name', 'category_id',
'template_id', 'visit_counter', 'comment_counter', 'date_post')
values ('Yet another post bis 23', 1, 1, 0, 0, "2007-01-17 11:23:01");
