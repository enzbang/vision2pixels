--  Create users

insert into user ("login", "password", "email", "admin", created)
       values ('enzbang', 'password', 'v2p@ramonat.fr', "TRUE", datetime(current_timestamp, '-2 days'));
insert into user ("login", "password", "email", "admin", created)
       values ('turbo', 'turbopass', 'v2p@obry.net', "TRUE", datetime(current_timestamp, '-12 days'));

--  Create forums

insert into forum values (1, "Forum photographies", "TRUE", "TRUE");
insert into forum values (2, "Forum matériel", "FALSE", "FALSE");
insert into forum values (3, "Forum site v2p", "FALSE", "FALSE");

--  Create categories

insert into category values (1, 1, 'Abstrait');
insert into category values (2, 1, 'Macro/Animaux');
insert into category values (3, 1, 'Nature morte');
insert into category values (4, 1, 'Mode');
insert into category values (5, 1, 'Nu');
insert into category values (6, 1, 'Paysage');
insert into category values (7, 1, 'Portrait');

insert into category values (8, 2, 'Vente / Achat');
insert into category values (9, 2, 'Discussion');

insert into category values (10, 3, 'Bug');
insert into category values (11, 3, 'Idée');

--  Create criteria

insert into criteria values (1, 'Composition');
insert into criteria values (2, 'Focus');
insert into criteria values (3, 'Lumière');

--  Create templates

insert into template values (1, 'mytemplate', 'V2P Template');
