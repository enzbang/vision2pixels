--  Create users

insert into user ("login", "password", "email", "admin", created)
       values ('enzbang', 'password', 'v2p@ramonat.fr', "TRUE", datetime(current_timestamp, '-2 days'));
insert into user ("login", "password", "email", "admin", created)
       values ('turbo', 'turbopass', 'v2p@obry.net', "TRUE", datetime(current_timestamp, '-12 days'));

--  Create forums

insert into forum
       values (1, "Photographies", "", datetime(current_timestamp), "TRUE", "TRUE");
insert into forum
       values (2, "Techniques", "Discussions autour des aspects techniques de la photographie comme la gestion de la lumière ou la profondeur de champ mais aussi les questions sur les logiciels de développement.", datetime(current_timestamp), "FALSE", "FALSE");
insert into forum
       values (3, "Matériels", "Discussions autour du matériel photographique, nouveautés, achat, vente...", datetime(current_timestamp), "FALSE", "FALSE");
insert into forum
       values (4, "Site v2p", "Discussions à propos du site lui même, les évolutions possibles, les rapports de bug.", datetime(current_timestamp), "FALSE", "FALSE");
insert into forum
       values (5, "Le Bar", "Discussions à bâtons rompus jusqu'au bout de la nuit...", datetime(current_timestamp), "FALSE", "FALSE");
insert into forum
       values (6, "Thèmes", "Animation autour d'un thème", datetime(current_timestamp), "FALSE", "FALSE");

--  Create categories

insert into category values (1, 1, 'Abstrait');
insert into category values (2, 1, 'Macro/Animaux');
insert into category values (3, 1, 'Nature morte');
insert into category values (4, 1, 'Mode');
insert into category values (5, 1, 'Nu');
insert into category values (6, 1, 'Paysage');
insert into category values (7, 1, 'Portrait');

insert into category values (8, 3, 'Vente / Achat');
insert into category values (9, 3, 'Discussion');

insert into category values (12, 2, 'Divers');

insert into category values (13, 1, 'Nature');
insert into category values (14, 1, 'Scène de rue');
insert into category values (15, 1, 'Architecture/Monuments');
insert into category values (16, 1, 'Sport');
insert into category values (17, 1, 'Reportage');
insert into category values (19, 1, 'Spectacle');

insert into category values (10, 4, 'Bug');
insert into category values (11, 4, 'Idée');
insert into category values (18, 4, 'Divers');

insert into category values (20, 5, 'Divers');

insert into category values (21, 6, 'Préparation');
insert into category values (22, 6, 'Thème');

--  Create criteria

insert into criteria values (1, 'Artistique');
insert into criteria values (2, 'Technique');

--  Create templates

insert into template values (1, 'mytemplate', 'V2P Template');
