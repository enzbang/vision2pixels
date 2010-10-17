--  Create users

insert into user ("login", "password", "email", "admin", "created")
       values ('enzbang', 'password', 'v2p@ramonat.fr', "TRUE", datetime(current_timestamp, '-2 days'));
insert into user ("login", "password", "email", "admin", "created")
       values ('turbo', 'turbopass', 'v2p@obry.net', "TRUE", datetime(current_timestamp, '-12 days'));
insert into user ("login", "password", "email", "admin", "created")
       values ('test', 'test', 'test@whatever.fr', "FALSE", datetime(current_timestamp));
insert into user ("login", "password", "email", "admin", "created")
       values ('x"x', 'x"x', 'xx@whatever.fr', "FALSE", datetime(current_timestamp));

insert into user_preferences ("user_login", "accept_private_message")
       values ('turbo', "TRUE");

--  User page

--  A trigger creates empty user pages, delete them first

delete from user_page where user_login='turbo';
delete from user_page where user_login='enzbang';
delete from user_page where user_login='test';

insert into user_page ("user_login", "content", "content_html")
   values ('turbo',
           'web page for turbo photographer using Nikon.',
           '<p>web page for turbo photographer using Nikon.</p>');

insert into user_page ("user_login", "content", "content_html")
   values ('enzbang',
           'web page for enzbang photographer using Canon.',
           '<p>web page for enzbang photographer using Canon.</p>');

insert into user_page ("user_login", "content", "content_html")
   values ('test',
           'web page for test photographer using Nikon and Canon. animal',
           '<p>web page for test photographer using Nikon and Canon.</p>');

--  Create forums

insert into forum values (1, "Forum photographies",
       datetime(current_timestamp), "TRUE", "TRUE");
insert into forum values (2, "Forum matériel",
       datetime(current_timestamp, '-5.0003 days'), "FALSE", "FALSE");

--  Create categories

insert into category values (1, 1, 'Portrait');
insert into category values (2, 1, 'Paysage');
insert into category values (3, 1, 'Macro/Animaux');
insert into category values (4, 1, 'Nature morte');
insert into category values (5, 1, 'Abstrait');

insert into category values (6, 2, 'MatCat1');
insert into category values (7, 2, 'MatCat2');

--  Create criteria

insert into criteria values (1, 'Artistique');
insert into criteria values (2, 'Technique');

--  Create templates

insert into template values (1, 'mytemplate', 'V2P Template');

--  Post comments

insert into post
    values (1, 'vends canon 350D', NULL, 'en tres bon état', 6, datetime(current_timestamp, '-1.0010 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (2, 'vends nikon D200', NULL, 'en tres mauvais état', 6, datetime(current_timestamp, '-2.0008 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (3, 'vends pentax', NULL, 'avec des pellicules Kodacolor 100 - 24 poses', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (4, 'vends 4', NULL, '4', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (5, 'vends 5', NULL, '5', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (6, 'vends 6', NULL, '6', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (7, 'vends 7', NULL, '7', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (8, 'vends 8', NULL, '8', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (9, 'vends 9', NULL, '9', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (10, 'vends 10', NULL, '10', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (11, 'vends 11', NULL, '11', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (12, 'vends 12', NULL, '12', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (13, 'vends 13', NULL, '13', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (14, 'vends 14', NULL, '14', 6, datetime(current_timestamp, '-5.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
    values (15, 'vends 15', NULL, '15', 6, datetime(current_timestamp, '-40 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");

insert into post
    values (54, 'un camion', 1, 'commentaire du camion', 2, datetime(current_timestamp, '-2.999 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (55, 'Cheminées', 2, 'commentaire pour cheminées', 2, datetime(current_timestamp, '-2.0009 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (56, 'Manger une fraise', 3, 'commentaire pour manger une fraise', 1, datetime(current_timestamp, '-2.0008 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (57, 'Ponton', 4, 'commentaire pour ponton', 2, datetime(current_timestamp, '-2.0007 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (58, 'By night', 5, 'commentaire pour by night', 2, datetime(current_timestamp, '-2.0006 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (59, 'Blé', 6, 'commentaire pour blé', 2, datetime(current_timestamp, '-2.0005 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (60, 'Ribik', 7, 'commentaire pour rubik', 4, datetime(current_timestamp, '-2.0004 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (61, 'Stylo', 8, 'commentaire pour stylo', 4, datetime(current_timestamp, '-2.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (62, 'Fire', 9, 'commentaire pour fire', 4, datetime(current_timestamp, '-2.0002 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (63, 'Fleurs', 10, 'commentaire pour fleurs', 3, datetime(current_timestamp, '-2.0001 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (64, 'Chaises', 11, 'commentaire pour chaises', 4, datetime(current_timestamp, '-5.0010 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (65, 'Kiwi', 12, 'commentaire pour kiwi', 4, datetime(current_timestamp, '-5.0009 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (66, 'La Grande Roue', 13, 'commentaire la grande roue', 5, datetime(current_timestamp, '-5.0008 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (67, 'Un soleil ?', 14, 'commentaire un soleil ?', 5, datetime(current_timestamp, '-5.0007 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (68, 'Eglise', 15, 'commentaire pour église', 2, datetime(current_timestamp, '-7.0050 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (69, '4', 16, 'commentaire pour 4', 5, datetime(current_timestamp, '-7.0049 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (70, 'Livre', 17, 'commentaire pour livre', 4, datetime(current_timestamp, '-7.0048 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (71, 'Figée', 18, 'commentaire pour figée', 4, datetime(current_timestamp, '-7.0047 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (72, 'Lock', 19, 'commentaire pour lock', 4, datetime(current_timestamp, '-7.0046 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (73, 'Manque d''eau', 20, 'commentaire pour manque d''eau', 2, datetime(current_timestamp, '-7.0045 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (74, 'Violon', 21, 'commentaire pour violon', 4, datetime(current_timestamp, '-7.0044 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (75, 'Parlons ensemble', 22, 'commentaire pour parlons ensemble', 4, datetime(current_timestamp, '-7.0043 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (76, 'Couché de soleil', 23, 'commentaire pour couché de soleil', 2, datetime(current_timestamp, '-8.0050 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (77, 'Oeuf', 24, 'commentaire pour un oeuf', 4, datetime(current_timestamp, '-8.0049 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (78, 'Envie de sport', 25, 'commentaire pour envie de sport', 2, datetime(current_timestamp, '-8.0048 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (79, 'Entre deux eaux', 26, 'commentaire pour entre deux eaux', 3, datetime(current_timestamp, '-8.0047 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (80, 'Ca passe par là', 27, 'commentaire pour ça passe par là', 5, datetime(current_timestamp, '-8.0046 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (81, 'Liens', 28, 'commentaire pour liens', 4, datetime(current_timestamp, '-8.0045 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (82, 'Même pas dans mes cauchemars', 29, 'commentaire pour même pas dans mes cauchemars', 3, datetime(current_timestamp, '-8.0044 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (83, 'Belles dents', 30, 'commentaire pour belles dents', 3, datetime(current_timestamp, '-8.0043 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (84, 'Perdu ?', 31, 'commentaire pour perdu', 3, datetime(current_timestamp, '-1.0060 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (85, 'Port', 32, 'commentaire pour port', 2, datetime(current_timestamp, '-1.0059 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (86, 'Notre guide', 33, 'commentaire pour notre guide', 2, datetime(current_timestamp, '-1.0058 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (87, 'Désséché', 34, 'commentaire pour désséché', 5, datetime(current_timestamp, '-0.0050 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (88, 'On ne pousse pas', 35, 'commentaire pour on ne pousse pas', 2, datetime(current_timestamp, '-0.0049 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (89, 'Invasion', 36, 'commentaire pour invasion', 1, datetime(current_timestamp, '-0.0048 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post
   values (90, 'Haut en couleurs', 37, 'commentaire pour haut en couleurs', 1, datetime(current_timestamp, '-1.0050 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (91, 'Bus', 38, NULL, 2, datetime(current_timestamp, '-1.0049 days'), datetime(current_timestamp), 1, 1, 0, "FALSE");
insert into post values (92, 'Tour E', 39, NULL, 2, datetime(current_timestamp, '-1.0048 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (93, 'Mine de rien', 40, NULL, 3, datetime(current_timestamp, '-1.0047 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (94, 'Cabane ', 41, NULL, 2, datetime(current_timestamp, '-1.0046 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (95, 'Saturne', 42, NULL, 2, datetime(current_timestamp, '-1.0045 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (96, 'Changement de direction', 43, NULL, 5, datetime(current_timestamp, '-1.0044 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (97, 'Home sweet home', 44, NULL, 5, datetime(current_timestamp, '-1.0043 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (98, 'Feuille', 45, NULL, 3, datetime(current_timestamp, '-1.0042 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (99, 'United color of', 46, NULL, 3, datetime(current_timestamp, '-1.0041 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (100, 'Abstract', 47, NULL, 5, datetime(current_timestamp, '-1.0040 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (101, 'Batracien', 48, NULL, 3, datetime(current_timestamp, '-1.0039 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (102, 'Campagne', 49, NULL, 2, datetime(current_timestamp, '-1.0038 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (103, 'Rouages', 50, NULL, 3, datetime(current_timestamp, '-1.0037 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (104, 'Without you I''m nothing', 51, NULL, 3, datetime(current_timestamp, '-1.0036 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (105, 'Le train en marche', 52, NULL, 2, datetime(current_timestamp, '-1.0035 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (106, 'A quai', 53, NULL, 2, datetime(current_timestamp, '-1.0034 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (107, 'L''Europe', 54, NULL, 2, datetime(current_timestamp, '-1.0033 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (108, 'Motif', 55, NULL, 5, datetime(current_timestamp, '-1.0032 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (109, 'Balle', 56, NULL, 3, datetime(current_timestamp, '-1.0031 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (110, 'Neige', 57, NULL, 2, datetime(current_timestamp, '-1.0030 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (111, 'Espagne', 58, NULL, 2, datetime(current_timestamp, '-1.0029 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (112, 'Quel ciel !', 59, NULL, 2, datetime(current_timestamp, '-1.0028 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (113, 'Pas à pas', 60, NULL, 2, datetime(current_timestamp, '-1.0027 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (114, 'H2O', 61, NULL, 5, datetime(current_timestamp, '-1.0026 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (115, 'Touches', 62, NULL, 5, datetime(current_timestamp, '-1.0025 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (116, 'GDF', 63, NULL, 5, datetime(current_timestamp, '-1.0024 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (117, 'Gourmandises', 64, NULL, 5, datetime(current_timestamp, '-1.0023 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (118, 'London', 65, NULL, 2, datetime(current_timestamp, '-1.0022 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (119, 'On the road, zooming again', 66, NULL, 5, datetime(current_timestamp, '-1.0021 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (120, 'Quel gland  !', 67, NULL, 4, datetime(current_timestamp, '-1.0020 days'), datetime(current_timestamp), 1, 1, 0, "FALSE");
insert into post values (121, 'Cascades', 68, NULL, 2, datetime(current_timestamp, '-1.0019 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (122, 'Ecologie ?', 69, NULL, 5, datetime(current_timestamp, '-1.0018 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (123, 'While My Guitar Gently Weeps', 70, NULL, 3, datetime(current_timestamp, '-1.0017 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (124, 'Parapentiste', 71, NULL, 1, datetime(current_timestamp, '-1.0016 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (125, 'En chemin', 72, NULL, 2, datetime(current_timestamp, '-1.0015 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (126, 'Smoking... No smoking', 73, NULL, 3, datetime(current_timestamp, '-1.0014 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (127, 'Vert', 74, NULL, 4, datetime(current_timestamp, '-1.0013 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (128, 'Spip', 75, NULL, 3, datetime(current_timestamp, '-1.0012 days'), datetime(current_timestamp), 1, 1, 0, "FALSE");
insert into post values (129, 'This way', 76, NULL, 5, datetime(current_timestamp, '-1.0011 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (130, 'Bridge', 77, NULL, 2, datetime(current_timestamp, '-1.0010 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (131, 'Road', 78, NULL, 2, datetime(current_timestamp, '-1.0009 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (132, 'En troupeau', 79, NULL, 3, datetime(current_timestamp, '-1.0008 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (133, 'Arc en ciel', 80, NULL, 2, datetime(current_timestamp, '-1.0007 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (134, 'Yoda', 81, NULL, 1, datetime(current_timestamp, '-1.0006 days'), datetime(current_timestamp), 1, 1, 0, "FALSE");
insert into post values (135, 'Côtes', 82, NULL, 2, datetime(current_timestamp, '-1.0005 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (136, 'Keys', 83, NULL, 5, datetime(current_timestamp, '-1.0004 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (137, 'Un éléphant ça trompe...', 84, NULL, 3, datetime(current_timestamp, '-1.0003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (138, 'Ah Parisssse', 85, NULL, 2, datetime(current_timestamp, '-1.0002 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (139, 'Coucher de soleil', 86, NULL, 2, datetime(current_timestamp, '-1.0001 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (140, 'Rides', 87, NULL, 4, datetime(current_timestamp, '-1.0000 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");
insert into post values (141, 'Hissez haut...', 88, NULL, 2, datetime(current_timestamp, '-0.003 days'), datetime(current_timestamp), 1, 0, 0, "FALSE");

--  User post

insert into user_post values ('enzbang', 1);
insert into user_post values ('turbo', 2);
insert into user_post values ('turbo', 3);
insert into user_post values ('turbo', 4);
insert into user_post values ('turbo', 5);
insert into user_post values ('turbo', 6);
insert into user_post values ('turbo', 7);
insert into user_post values ('turbo', 8);
insert into user_post values ('turbo', 9);
insert into user_post values ('turbo', 10);
insert into user_post values ('turbo', 11);
insert into user_post values ('turbo', 12);
insert into user_post values ('turbo', 13);
insert into user_post values ('turbo', 14);
insert into user_post values ('turbo', 15);

insert into user_post values ('turbo', 54);
insert into user_post values ('turbo', 55);
insert into user_post values ('turbo', 56);
insert into user_post values ('turbo', 57);
insert into user_post values ('turbo', 58);
insert into user_post values ('turbo', 59);
insert into user_post values ('turbo', 60);
insert into user_post values ('turbo', 61);
insert into user_post values ('turbo', 62);
insert into user_post values ('turbo', 63);
insert into user_post values ('turbo', 64);
insert into user_post values ('turbo', 65);
insert into user_post values ('turbo', 66);
insert into user_post values ('turbo', 67);
insert into user_post values ('turbo', 68);
insert into user_post values ('turbo', 69);
insert into user_post values ('turbo', 70);
insert into user_post values ('turbo', 71);
insert into user_post values ('turbo', 72);
insert into user_post values ('turbo', 73);
insert into user_post values ('turbo', 74);
insert into user_post values ('turbo', 75);
insert into user_post values ('turbo', 76);
insert into user_post values ('turbo', 77);
insert into user_post values ('turbo', 78);
insert into user_post values ('turbo', 79);
insert into user_post values ('turbo', 80);
insert into user_post values ('turbo', 81);
insert into user_post values ('turbo', 82);
insert into user_post values ('turbo', 83);
insert into user_post values ('turbo', 84);
insert into user_post values ('turbo', 85);
insert into user_post values ('turbo', 86);
insert into user_post values ('turbo', 87);
insert into user_post values ('turbo', 88);
insert into user_post values ('turbo', 89);
insert into user_post values ('turbo', 90);
insert into user_post values ('enzbang', 91);
insert into user_post values ('enzbang', 92);
insert into user_post values ('enzbang', 93);
insert into user_post values ('enzbang', 94);
insert into user_post values ('enzbang', 95);
insert into user_post values ('enzbang', 96);
insert into user_post values ('enzbang', 97);
insert into user_post values ('enzbang', 98);
insert into user_post values ('enzbang', 99);
insert into user_post values ('enzbang', 100);
insert into user_post values ('enzbang', 101);
insert into user_post values ('enzbang', 102);
insert into user_post values ('enzbang', 103);
insert into user_post values ('enzbang', 104);
insert into user_post values ('enzbang', 105);
insert into user_post values ('enzbang', 106);
insert into user_post values ('enzbang', 107);
insert into user_post values ('enzbang', 108);
insert into user_post values ('enzbang', 109);
insert into user_post values ('enzbang', 110);
insert into user_post values ('enzbang', 111);
insert into user_post values ('enzbang', 112);
insert into user_post values ('enzbang', 113);
insert into user_post values ('enzbang', 114);
insert into user_post values ('enzbang', 115);
insert into user_post values ('enzbang', 116);
insert into user_post values ('enzbang', 117);
insert into user_post values ('enzbang', 118);
insert into user_post values ('enzbang', 119);
insert into user_post values ('enzbang', 120);
insert into user_post values ('enzbang', 121);
insert into user_post values ('enzbang', 122);
insert into user_post values ('enzbang', 123);
insert into user_post values ('enzbang', 124);
insert into user_post values ('enzbang', 125);
insert into user_post values ('enzbang', 126);
insert into user_post values ('enzbang', 127);
insert into user_post values ('enzbang', 128);
insert into user_post values ('enzbang', 129);
insert into user_post values ('enzbang', 130);
insert into user_post values ('enzbang', 131);
insert into user_post values ('enzbang', 132);
insert into user_post values ('enzbang', 133);
insert into user_post values ('enzbang', 134);
insert into user_post values ('enzbang', 135);
insert into user_post values ('enzbang', 136);
insert into user_post values ('enzbang', 137);
insert into user_post values ('enzbang', 138);
insert into user_post values ('enzbang', 139);
insert into user_post values ('enzbang', 140);
insert into user_post values ('enzbang', 141);

--  Photos

insert into photo
   values (1, '2007/Forum photographies/Paysage/200702011055-0.1.jpg',
           532, 800, 532, 800, 150, 150, 82558);
insert into photo
   values (2, '2007/Forum photographies/Paysage/200702011056-1.1x2.jpg',
           532, 800, 532, 800, 150, 150, 38172);
insert into photo
   values (3, '2007/Forum photographies/Portrait/200702011057-2.5.jpg',
           595, 800, 595, 800, 150, 150, 41123);
insert into photo
   values (4, '2007/Forum photographies/Paysage/200702011102-3.6.jpg',
           600, 800, 600, 800, 150, 150, 83107);
insert into photo
   values (5, '2007/Forum photographies/Paysage/200702011103-4.8.jpg',
           532, 800, 532, 800, 150, 150, 52925);
insert into photo
   values (6, '2007/Forum photographies/Paysage/200702011103-5.9.jpg',
           600, 800, 600, 800, 150, 150, 94582);
insert into photo
   values (7, '2007/Forum photographies/Nature morte/200702011104-6.9x.jpg',
           600, 800, 600, 800, 150, 150, 28457);
insert into photo
   values (8, '2007/Forum photographies/Nature morte/200702011105-7.11.jpg',
           533, 800, 533, 800, 150, 150, 29558);
insert into photo
   values (9, '2007/Forum photographies/Nature morte/200702011106-8.12.jpg',
           600, 764, 600, 764, 150, 150, 50060);
insert into photo
   values (10, '2007/Forum photographies/Macro/Animaux/200702011203-9.15.jpg',
           600, 800, 600, 800, 150, 150, 134700);
insert into photo
   values (11, '2007/Forum photographies/Nature morte/200702011208-10.23.jpg',
           531, 800, 531, 800, 150, 150, 58024);
insert into photo
   values (12, '2007/Forum photographies/Nature morte/200702011209-11.25.jpg',
           466, 800, 466, 800, 150, 150, 37296);
insert into photo
   values (13, '2007/Forum photographies/Abstrait/200702011210-12.25x.jpg',
           549, 800, 549, 800, 150, 150, 93859);
insert into photo
   values (14, '2007/Forum photographies/Abstrait/200702011211-13.29.jpg',
           600, 800, 600, 800, 150, 150, 42118);
insert into photo
   values (15, '2007/Forum photographies/Paysage/200702011212-14.31.jpg',
           593, 800, 593, 800, 150, 150, 73722);
insert into photo
   values (16, '2007/Forum photographies/Abstrait/200702011213-15.38.jpg',
           600, 800, 600, 800, 150, 150, 43389);
insert into photo
   values (17, '2007/Forum photographies/Nature morte/200702011214-16.48.jpg',
           532, 800, 532, 800, 150, 150, 52693);
insert into photo
   values (18, '2007/Forum photographies/Nature morte/200702011214-17.52.jpg',
           600, 800, 600, 800, 150, 150, 43914);
insert into photo
   values (19, '2007/Forum photographies/Nature morte/200702011215-18.53.jpg',
           600, 800, 600, 800, 150, 150, 55107);
insert into photo
   values (20, '2007/Forum photographies/Paysage/200702011216-19.55.jpg',
           600, 800, 600, 800, 150, 150, 33503);
insert into photo
   values (21, '2007/Forum photographies/Nature morte/200702011216-20.58.jpg',
           558, 800, 558, 800, 150, 150, 54012);
insert into photo
   values (22, '2007/Forum photographies/Nature morte/200702011217-21.61.jpg',
           800, 600, 800, 600, 150, 150, 43378);
insert into photo
   values (23, '2007/Forum photographies/Paysage/200702011217-22.71.jpg',
           800, 600, 800, 600, 150, 150, 143269);
insert into photo
   values (24, '2007/Forum photographies/Nature morte/200702011218-23.73.jpg',
           600, 600, 600, 600, 150, 150, 14464);
insert into photo
   values (25, '2007/Forum photographies/Paysage/200702011219-24.91.jpg',
           600, 800, 600, 800, 150, 150, 78384);
insert into photo
   values (26, '2007/Forum photographies/Macro/Animaux/200702011219-25.106.jpg',
           600, 788, 600, 788, 150, 150, 54892);
insert into photo
   values (27, '2007/Forum photographies/Abstrait/200702011220-26.110.jpg',
           600, 800, 600, 800, 150, 150, 97441);
insert into photo
   values (28, '2007/Forum photographies/Nature morte/200702011221-27.AGRIC055.jpg',
           800, 640, 800, 640, 150, 150, 87672);
insert into photo
   values (29, '2007/Forum photographies/Macro/Animaux/200702011222-28.ANIMAUX089.jpg',
           600, 750, 600, 750, 150, 150, 39951);
insert into photo
   values (30, '2007/Forum photographies/Macro/Animaux/200702011222-29.ANIMAUX113.jpg',
           600, 750, 600, 750, 150, 150, 73108);
insert into photo
   values (31, '2007/Forum photographies/Macro/Animaux/200702011223-30.ANIMAUX114.jpg',
           600, 750, 600, 750, 150, 150, 71637);
insert into photo
   values (32, '2007/Forum photographies/Paysage/200702011223-31.ARCHIT001.jpg',
           600, 750, 600, 750, 150, 150, 89270);
insert into photo
   values (33, '2007/Forum photographies/Paysage/200702011224-32.ARCHT072.jpg',
           750, 600, 750, 600, 150, 150, 25293);
insert into photo
   values (34, '2007/Forum photographies/Abstrait/200702011225-33.FONDS009.jpg',
           600, 750, 600, 750, 150, 150, 90927);
insert into photo
   values (35, '2007/Forum photographies/Paysage/200702011226-34.PERS008.jpg',
           600, 750, 600, 750, 150, 150, 59954);
insert into photo
   values (36, '2007/Forum photographies/Portrait/200702011227-35.PERS012.jpg',
           600, 750, 600, 750, 150, 150, 103911);
insert into photo
   values (37, '2007/Forum photographies/Portrait/200702011227-36.PERS024.jpg',
           600, 750, 600, 750, 150, 150, 72551);
insert into photo values (38, '2007/Forum photographies/Paysage/200702012114-0.conv_1.jpg',
               600, 800, 600, 800, 150, 150, 72762);
insert into photo values (39, '2007/Forum photographies/Paysage/200702012140-1.conv_4.jpg',
               600, 800, 600, 800, 150, 150, 146591);
insert into photo values (40, '2007/Forum photographies/Macro/Animaux/200702012140-2.conv_8.jpg',
               533, 800, 533, 800, 150, 150, 37338);
insert into photo values (41, '2007/Forum photographies/Paysage/200702012141-3.conv_10.jpg',
               533, 800, 533, 800, 150, 150, 147253);
insert into photo values (42, '2007/Forum photographies/Paysage/200702012141-4.conv_10b.jpg',
               531, 800, 531, 800, 150, 150, 113581);
insert into photo values (43, '2007/Forum photographies/Abstrait/200702012142-5.conv_11.jpg',
               800, 532, 800, 532, 150, 150, 56176);
insert into photo values (44, '2007/Forum photographies/Abstrait/200702012143-6.conv_11b.jpg',
               600, 800, 600, 800, 150, 150, 65582);
insert into photo values (45, '2007/Forum photographies/Macro/Animaux/200702012143-7.conv_12.jpg',
               599, 800, 599, 800, 150, 150, 77307);
insert into photo values (46, '2007/Forum photographies/Macro/Animaux/200702012144-8.conv_12b.jpg',
               600, 800, 600, 800, 150, 150, 123416);
insert into photo values (47, '2007/Forum photographies/Abstrait/200702012144-9.conv_13.jpg',
               600, 800, 600, 800, 150, 150, 94453);
insert into photo values (48, '2007/Forum photographies/Macro/Animaux/200702012145-10.conv_17.jpg',
               600, 800, 600, 800, 150, 150, 112182);
insert into photo values (49, '2007/Forum photographies/Paysage/200702012145-11.conv_19.jpg',
               533, 800, 533, 800, 150, 150, 85781);
insert into photo values (50, '2007/Forum photographies/Macro/Animaux/200702012146-12.conv_25.jpg',
               600, 800, 600, 800, 150, 150, 137316);
insert into photo values (51, '2007/Forum photographies/Macro/Animaux/200702012146-13.conv_25b.jpg',
               533, 800, 533, 800, 150, 150, 51365);
insert into photo values (52, '2007/Forum photographies/Paysage/200702012147-14.conv_30.jpg',
               800, 533, 800, 533, 150, 150, 60825);
insert into photo values (53, '2007/Forum photographies/Paysage/200702012147-15.conv_31.jpg',
               533, 800, 533, 800, 150, 150, 95442);
insert into photo values (54, '2007/Forum photographies/Paysage/200702012148-16.conv_32.jpg',
               600, 800, 600, 800, 150, 150, 99593);
insert into photo values (55, '2007/Forum photographies/Abstrait/200702012148-17.conv_34.jpg',
               524, 800, 524, 800, 150, 150, 79084);
insert into photo values (56, '2007/Forum photographies/Macro/Animaux/200702012149-18.conv_36.jpg',
               600, 800, 600, 800, 150, 150, 20425);
insert into photo values (57, '2007/Forum photographies/Paysage/200702012149-19.conv_39.jpg',
               533, 800, 533, 800, 150, 150, 149062);
insert into photo values (58, '2007/Forum photographies/Paysage/200702012149-20.conv_40.jpg',
               600, 800, 600, 800, 150, 150, 100181);
insert into photo values (59, '2007/Forum photographies/Paysage/200702012150-21.conv_40b.jpg',
               657, 800, 657, 800, 150, 150, 102548);
insert into photo values (60, '2007/Forum photographies/Paysage/200702012150-22.conv_42.jpg',
               800, 533, 800, 533, 150, 150, 123293);
insert into photo values (61, '2007/Forum photographies/Abstrait/200702012151-23.conv_43.jpg',
               800, 600, 800, 600, 150, 150, 141948);
insert into photo values (62, '2007/Forum photographies/Abstrait/200702012151-24.conv_46.jpg',
               600, 800, 600, 800, 150, 150, 74131);
insert into photo values (63, '2007/Forum photographies/Abstrait/200702012151-25.conv_49.jpg',
               600, 800, 600, 800, 150, 150, 39485);
insert into photo values (64, '2007/Forum photographies/Abstrait/200702012152-26.conv_49b.jpg',
               532, 800, 532, 800, 150, 150, 105675);
insert into photo values (65, '2007/Forum photographies/Paysage/200702012152-27.conv_55.jpg',
               600, 800, 600, 800, 150, 150, 123954);
insert into photo values (66, '2007/Forum photographies/Abstrait/200702012153-28.conv_57.jpg',
               532, 800, 532, 800, 150, 150, 87446);
insert into photo values (67, '2007/Forum photographies/Nature morte/200702012153-29.conv_58.jpg',
               500, 800, 500, 800, 150, 150, 85840);
insert into photo values (68, '2007/Forum photographies/Paysage/200702012154-30.conv_60.jpg',
               800, 533, 800, 533, 150, 150, 57675);
insert into photo values (69, '2007/Forum photographies/Abstrait/200702012155-31.conv_63.jpg',
               600, 800, 600, 800, 150, 150, 61868);
insert into photo values (70, '2007/Forum photographies/Macro/Animaux/200702012155-32.conv_67.jpg',
               600, 800, 600, 800, 150, 150, 87149);
insert into photo values (71, '2007/Forum photographies/Portrait/200702012156-33.conv_70.jpg',
               800, 531, 800, 531, 150, 150, 47871);
insert into photo values (72, '2007/Forum photographies/Paysage/200702012156-34.conv_71.jpg',
               533, 800, 533, 800, 150, 150, 192053);
insert into photo values (73, '2007/Forum photographies/Macro/Animaux/200702012157-35.conv_80.jpg',
               523, 800, 523, 800, 150, 150, 35783);
insert into photo values (74, '2007/Forum photographies/Nature morte/200702012157-36.conv_92.jpg',
               600, 800, 600, 800, 150, 150, 180136);
insert into photo values (75, '2007/Forum photographies/Macro/Animaux/200702012157-37.conv_93.jpg',
               587, 800, 587, 800, 150, 150, 138881);
insert into photo values (76, '2007/Forum photographies/Abstrait/200702012158-38.conv_96.jpg',
               600, 800, 600, 800, 150, 150, 188896);
insert into photo values (77, '2007/Forum photographies/Paysage/200702012158-39.conv_99.jpg',
               532, 800, 532, 800, 150, 150, 117796);
insert into photo values (78, '2007/Forum photographies/Paysage/200702012159-40.conv_105.jpg',
               800, 532, 800, 532, 150, 150, 88058);
insert into photo values (79, '2007/Forum photographies/Macro/Animaux/200702012159-41.conv_ANIMX031.jpg',
               526, 800, 526, 800, 150, 150, 37963);
insert into photo values (80, '2007/Forum photographies/Paysage/200702012159-42.conv_ARBRE017.jpg',
               640, 800, 640, 800, 150, 150, 34189);
insert into photo values (81, '2007/Forum photographies/Portrait/200702012200-43.conv_ARBRE023.jpg',
               640, 800, 640, 800, 150, 150, 66731);
insert into photo values (82, '2007/Forum photographies/Paysage/200702012200-44.conv_COTES064.jpg',
               529, 800, 529, 800, 150, 150, 49048);
insert into photo values (83, '2007/Forum photographies/Abstrait/200702012201-45.conv_MAISN002.jpg',
               543, 800, 543, 800, 150, 150, 22580);
insert into photo values (84, '2007/Forum photographies/Macro/Animaux/200702012201-46.conv_MONUM113.jpg',
               527, 800, 527, 800, 150, 150, 49269);
insert into photo values (85, '2007/Forum photographies/Paysage/200702012202-47.conv_PARIS008.jpg',
               800, 506, 800, 506, 150, 150, 30177);
insert into photo values (86, '2007/Forum photographies/Paysage/200702012202-48.conv_SPTLS046.jpg',
               800, 529, 800, 529, 150, 150, 49362);
insert into photo values (87, '2007/Forum photographies/Nature morte/200702012202-49.conv_TEX009.jpg',
               533, 800, 533, 800, 150, 150, 78529);
insert into photo values (88, '2007/Forum photographies/Paysage/200702012203-50.conv_TRSPT066.jpg',
               800, 641, 800, 641, 150, 150, 54089);
insert into photo values (89, '2007/Forum photographies/Paysage/200702012203-50.conv_TRSPT066x2.jpg',
               800, 641, 800, 641, 150, 150, 54089);
insert into photo values (90, '2007/Forum photographies/Paysage/200702012203-50.conv_TRSPT066x3.jpg',
               800, 641, 800, 641, 150, 150, 54089);

--  Comments
insert into comment values (1, datetime(current_timestamp, '-5.0009 days'), NULL, 'enzbang', NULL, '<p>See <a href=''http://en.wikipedia.org/wiki/Rubik%27s_cube'' rel=''nofollow''>http://en.wikipedia.org/wiki/Rubik%27s_cube</a> if you''re interested...</p>', NULL, "FALSE");
insert into comment values (2, datetime(current_timestamp, '-4.0700 days'), NULL, 'enzbang', NULL, '<p>Quel est cet animal ?</p>', NULL, "FALSE");
insert into comment values (3, datetime(current_timestamp, '-4.0009 days'), NULL, 'enzbang', NULL, '<p>Parque Nacional de Monegros ?</p>', NULL, "FALSE");
insert into comment values (4, datetime(current_timestamp, '-3.0809 days'), NULL, 'enzbang', NULL, '<p>Filtre polarisant ?</p>', NULL, "FALSE");
insert into comment values (5, datetime(current_timestamp, '-3.0009 days'), NULL, 'enzbang', NULL, '<p>Id&#195;&#169;e originale ! <em>bravo</em> </p>', NULL, "FALSE");
insert into comment values (6, datetime(current_timestamp, '-2.0709 days'), NULL, 'enzbang', NULL, '<p>Tiens les accents ne passent pas ? &#195;&#169;&#195;&#160;&#195;&#185;&#195;&#179;</p>', NULL, "FALSE");
insert into comment values (7, datetime(current_timestamp, '-2.0009 days'), NULL, 'enzbang', NULL, '<p>Hum... sans moi</p>', NULL, "FALSE");
insert into comment values (8, datetime(current_timestamp, '-1.0809 days'), NULL, 'enzbang', NULL, '<p>&#195;&#167;a penche !</p>', NULL, "FALSE");
insert into comment values (9, datetime(current_timestamp, '-1.0609 days'), NULL, 'enzbang', NULL, '<p>la m&#195;&#170;me avec un cadre ?</p>', NULL, "FALSE");
insert into comment values (10, datetime(current_timestamp, '-1.0509 days'), NULL, 'enzbang', NULL, '<p>J''aurais cadr&#195;&#169; l&#195;&#169;g&#195;&#168;rement plus &#195;&#160; droite</p>', NULL, "FALSE");
insert into comment values (11, datetime(current_timestamp, '-1.0409 days'), NULL, 'enzbang', NULL, '<p>D&#195;&#169;sol&#195;&#169; mais je n''accroche pas</p>', NULL, "FALSE");
insert into comment values (12, datetime(current_timestamp, '-1.0309 days'), NULL, 'enzbang', NULL, '<p>Good idea 4 this picture</p>', NULL, "FALSE");
insert into comment values (13, datetime(current_timestamp, '-1.0207 days'), NULL, 'enzbang', NULL, '<p>Un classique</p>', NULL, "FALSE");
insert into comment values (14, datetime(current_timestamp, '-1.0105 days'), NULL, 'enzbang', NULL, '<p>^_^</p>', NULL, "FALSE");
insert into comment values (15, datetime(current_timestamp, '-0.0505 days'), NULL, 'enzbang', NULL, '<p>Unlocked ?</p>', NULL, "FALSE");
insert into comment values (16, datetime(current_timestamp, '-0.0500 days'), NULL, 'enzbang', NULL, '<p>L''envie de fraise d&#195;&#169;j&#195;&#160; !</p>', NULL, "FALSE");
insert into comment values (17, datetime(current_timestamp, '-0.0009 days'), NULL, 'turbo', NULL, '<p>Alors qu''en pensez-vous?</p>', NULL, "FALSE");
insert into comment values (18, datetime(current_timestamp, '-0.0007 days'), NULL, 'enzbang', NULL, '<p>Bof!</p>', NULL, "FALSE");
insert into comment values (19, datetime(current_timestamp, '-0.0006 days'), NULL, 'turbo', NULL, '<p>Mais encore ?</p>', NULL, "FALSE");
insert into comment values (20, datetime(current_timestamp, '-0.0005 days'), NULL, 'turbo', NULL, '<p>j''aime!</p>', NULL, "FALSE");
insert into comment values (21, datetime(current_timestamp, '-0.0004 days'), NULL, 'turbo', NULL, '<p>Superbe</p>', NULL, "FALSE");
insert into comment values (22, datetime(current_timestamp, '-0.0003 days'), NULL, 'turbo', NULL, '<p>com en imafe</p>', 89, "FALSE");
insert into comment values (23, datetime(current_timestamp, '-0.0002 days'), NULL, 'test', NULL, '<p>prefere l''original</p>', NULL, "FALSE");
insert into comment values (24, datetime(current_timestamp, '-0.0001 days'), NULL, 'turbo', NULL, '<p>une autre proposition</p>', 90, "FALSE");

insert into post_comment values (60, 1);
insert into post_comment values (84, 2);
insert into post_comment values (87, 3);
insert into post_comment values (54, 4);
insert into post_comment values (64, 5);
insert into post_comment values (64, 6);
insert into post_comment values (68, 7);
insert into post_comment values (76, 8);
insert into post_comment values (77, 9);
insert into post_comment values (78, 10);
insert into post_comment values (81, 11);
insert into post_comment values (69, 12);
insert into post_comment values (70, 13);
insert into post_comment values (71, 14);
insert into post_comment values (72, 15);
insert into post_comment values (56, 16);
insert into post_comment values (89, 17);
insert into post_comment values (89, 18);
insert into post_comment values (89, 19);
insert into post_comment values (71, 20);
insert into post_comment values (141, 21);
insert into post_comment values (141, 22);
insert into post_comment values (141, 23);
insert into post_comment values (141, 24);

--  Reset the last_activity given the last date in the comment, this is
--  necessary because the insert just above will assign (trigger) the same
--  last_activity to all entries.

UPDATE post
   SET last_activity=(SELECT COALESCE ((SELECT MAX(comment.date)
                      FROM comment, post_comment
                      WHERE post_comment.comment_id = comment.id
                            AND post_comment.post_id = post.id),
                      post.date_post));

--  Insert rating

insert into rating values ("turbo", 141, 1, 5);
insert into rating values ("enzbang", 141, 1, 1);
insert into rating values ("test", 141, 1, 4);

insert into rating values ("enzbang", 71, 1, 4);
insert into rating values ("enzbang", 71, 2, 4);

insert into photo_of_the_week values (0, 0, 0, datetime(current_timestamp));
insert into user_photo_of_the_week values ("turbo", 141, 0);
insert into user_photo_of_the_week values ("enzbang", 54, 0);
insert into user_photo_of_the_week values ("enzbang", 55, 0);
insert into user_photo_of_the_week values ("enzbang", 56, 0);
insert into user_photo_of_the_week values ("enzbang", 57, 0);
insert into user_photo_of_the_week values ("enzbang", 58, 0);
insert into user_photo_of_the_week values ("enzbang", 59, 0);
insert into user_photo_of_the_week values ("enzbang", 60, 0);
insert into user_photo_of_the_week values ("enzbang", 61, 0);
insert into user_photo_of_the_week values ("enzbang", 140, 0);
insert into user_photo_of_the_week values ("test", 141, 0);
insert into user_photo_of_the_week values ("test", 140, 0);

insert into photo_of_the_week values (1, 67, 2.0, date(current_date, '-0.0003 days'));
insert into photo_of_the_week values (2, 87, 2.0, date(current_date, '-7.0003 days'));
insert into photo_of_the_week values (3, 90, 2.0, date(current_date, '-14.0003 days'));
insert into photo_of_the_week values (4, 99, 2.0, date(current_date, '-21.0003 days'));
insert into photo_of_the_week values (5, 134, 2.0, date(current_date, '-28.0003 days'));

--  EXIF

insert into photo_exif values
  (36, "2006:06:09 18:41:29", "NIKON CORPORATION", "NIKON D200", "1/160", "6.3", "No Flash", "105.0mm", "Auto", "Program AE", "Auto", "Multi-segment", "400");
insert into photo_exif values
  (14, "2006:06:09 18:41:29", "NIKON CORPORATION", "NIKON D200", "1/160", "2.8", "No Flash", "105.0mm", "Auto", "Program AE", "Auto", "Multi-segment", "200");

--  Insert metadata

insert into photo_metadata values (88, 46.2573, 4.30196, "N 46° 15' 26", "E 4° 18' 7");

update forum
       set last_activity = datetime(current_timestamp, '-5.0003 days')
       where forum.id = 2;

insert into user_to_validate
   ('login', 'password', 'email')
   values ('username1', '1', 'username1@obry.net');
insert into user_to_validate
   ('login', 'password', 'email')
   values ('username2', '2', 'username2@obry.net');
