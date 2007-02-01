--  Create users

insert into user values ('enzbang', 'password', 'v2p@ramonat.fr');
insert into user values ('turbo', 'turbopass', 'v2p@obry.net');

--  Create forums

insert into forum values (1, "Forum photographies");
insert into forum values (2, "Forum matériel");

--  Create categories

insert into category values (1, 1, 'Portrait');
insert into category values (2, 1, 'Paysage');
insert into category values (3, 1, 'Macro/Animaux');
insert into category values (4, 1, 'Nature morte');
insert into category values (5, 1, 'Abstrait');

insert into category values (6, 2, 'MatCat1');
insert into category values (7, 2, 'MatCat2');

--  Create templates

insert into template values (1, 'mytemplate', 'V2P Template');

--  Comment counter

create trigger update_comment_counter insert on comment
   begin
      update post set comment_counter=comment_counter + 1;
   end;

--  Post comments

INSERT INTO post
    VALUES(54, 'un camion', 1, 'commentaire du camion', 2, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(55, 'Cheminées', 2, 'commentaire pour cheminées', 2, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(56, 'Manger une fraise', 3, 'commentaire pour manger une fraise', 1, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(57, 'Ponton', 4, 'commentaire pour ponton', 2, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(58, 'By night', 5, 'commentaire pour by night', 2, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(59, 'Blé', 6, 'commentaire pour blé', 2, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(60, 'Ribik', 7, 'commentaire pour rubik', 4, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(61, 'Stylo', 8, 'commentaire pour stylo', 4, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(62, 'Fire', 9, 'commentaire pour fire', 4, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(63, 'Fleurs', 10, 'commentaire pour fleurs', 3, datetime(current_timestamp, '-2 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(64, 'Chaises', 11, 'commentaire pour chaises', 4, datetime(current_timestamp, '-5 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(65, 'Kiwi', 12, 'commentaire pour kiwi', 4, datetime(current_timestamp, '-5 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(66, 'La Grande Roue', 13, 'commentaire la grande roue', 5, datetime(current_timestamp, '-5 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(67, 'Un soleil ?', 14, 'commentaire un soleil ?', 5, datetime(current_timestamp, '-5 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(68, 'Eglise', 15, 'commentaire pour église', 2, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(69, '4', 16, 'commentaire pour 4', 5, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(70, 'Livre', 17, 'commentaire pour livre', 4, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(71, 'Figée', 18, 'commentaire pour figée', 4, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(72, 'Lock', 19, 'commentaire pour lock', 4, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(73, 'Manque d''eau', 20, 'commentaire pour manque d''eau', 2, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(74, 'Violon', 21, 'commentaire pour violon', 4, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(75, 'Parlons ensemble', 22, 'commentaire pour parlons ensemble', 4, datetime(current_timestamp, '-7 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(76, 'Couché de soleil', 23, 'commentaire pour couché de soleil', 2, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(77, 'Oeuf', 24, 'commentaire pour un oeuf', 4, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(78, 'Envie de sport', 25, 'commentaire pour envie de sport', 2, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(79, 'Entre deux eaux', 26, 'commentaire pour entre deux eaux', 3, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(80, 'Ca passe par là', 27, 'commentaire pour ça passe par là', 5, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(81, 'Liens', 28, 'commentaire pour liens', 4, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(82, 'Même pas dans mes cauchemars', 29, 'commentaire pour même pas dans mes cauchemars', 3, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(83, 'Belles dents', 30, 'commentaire pour belles dents', 3, datetime(current_timestamp, '-8 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(84, 'Perdu ?', 31, 'commentaire pour perdu', 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(85, 'Port', 32, 'commentaire pour port', 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(86, 'Notre guide', 33, 'commentaire pour notre guide', 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(87, 'Désséché', 34, 'commentaire pour désséché', 5, datetime(current_timestamp), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(88, 'On ne pousse pas', 35, 'commentaire pour on ne pousse pas', 2, datetime(current_timestamp), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(89, 'Invasion', 36, 'commentaire pour invasion', 1, datetime(current_timestamp), NULL, 1, 0, 0);
INSERT INTO post
   VALUES(90, 'Haut en couleurs', 37, 'commentaire pour haut en couleurs', 1, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(91, 'Bus', 38, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 1, 0);
INSERT INTO "post" VALUES(92, 'Tour E', 39, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(93, 'Mine de rien', 40, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(94, 'Cabane ', 41, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(95, 'Saturne', 42, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(96, 'Changement de direction', 43, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(97, 'Home sweet home', 44, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(98, 'Feuille', 45, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(99, 'United color of', 46, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(100, 'Abstract', 47, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(101, 'Batracien', 48, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(102, 'Campagne', 49, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(103, 'Rouages', 50, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(104, 'Without you I''m nothing', 51, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(105, 'Le train en marche', 52, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(106, 'A quai', 53, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(107, 'L''Europe', 54, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(108, 'Motif', 55, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(109, 'Balle', 56, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(110, 'Neige', 57, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(111, 'Espagne', 58, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(112, 'Quel ciel !', 59, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(113, 'Pas à pas', 60, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(114, 'H2O', 61, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(115, 'Touches', 62, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(116, 'GDF', 63, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(117, 'Gourmandises', 64, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(118, 'London', 65, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(119, 'On the road, zooming again', 66, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(120, 'Quel gland  !', 67, NULL, 4, datetime(current_timestamp, '-1 days'), NULL, 1, 1, 0);
INSERT INTO "post" VALUES(121, 'Cascades', 68, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(122, 'Ecologie ?', 69, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(123, 'While My Guitar Gently Weeps', 70, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(124, 'Parapentiste', 71, NULL, 1, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(125, 'En chemin', 72, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(126, 'Smoking... No smoking', 73, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(127, 'Vert', 74, NULL, 4, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(128, 'Spip', 75, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 1, 0);
INSERT INTO "post" VALUES(129, 'This way', 76, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(130, 'Bridge', 77, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(131, 'Road', 78, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(132, 'En troupeau', 79, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(133, 'Arc en ciel', 80, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(134, 'Yoda', 81, NULL, 1, datetime(current_timestamp, '-1 days'), NULL, 1, 1, 0);
INSERT INTO "post" VALUES(135, 'Côtes', 82, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(136, 'Keys', 83, NULL, 5, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(137, 'Un éléphant ça trompe...', 84, NULL, 3, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(138, 'Ah Parisssse', 85, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(139, 'Coucher de soleil', 86, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(140, 'Rides', 87, NULL, 4, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);
INSERT INTO "post" VALUES(141, 'Hissez haut...', 88, NULL, 2, datetime(current_timestamp, '-1 days'), NULL, 1, 0, 0);


--  User post

INSERT INTO user_post VALUES('turbo', 54);
INSERT INTO user_post VALUES('turbo', 55);
INSERT INTO user_post VALUES('turbo', 56);
INSERT INTO user_post VALUES('turbo', 57);
INSERT INTO user_post VALUES('turbo', 58);
INSERT INTO user_post VALUES('turbo', 59);
INSERT INTO user_post VALUES('turbo', 60);
INSERT INTO user_post VALUES('turbo', 61);
INSERT INTO user_post VALUES('turbo', 62);
INSERT INTO user_post VALUES('turbo', 63);
INSERT INTO user_post VALUES('turbo', 64);
INSERT INTO user_post VALUES('turbo', 65);
INSERT INTO user_post VALUES('turbo', 66);
INSERT INTO user_post VALUES('turbo', 67);
INSERT INTO user_post VALUES('turbo', 68);
INSERT INTO user_post VALUES('turbo', 69);
INSERT INTO user_post VALUES('turbo', 70);
INSERT INTO user_post VALUES('turbo', 71);
INSERT INTO user_post VALUES('turbo', 72);
INSERT INTO user_post VALUES('turbo', 73);
INSERT INTO user_post VALUES('turbo', 74);
INSERT INTO user_post VALUES('turbo', 75);
INSERT INTO user_post VALUES('turbo', 76);
INSERT INTO user_post VALUES('turbo', 77);
INSERT INTO user_post VALUES('turbo', 78);
INSERT INTO user_post VALUES('turbo', 79);
INSERT INTO user_post VALUES('turbo', 80);
INSERT INTO user_post VALUES('turbo', 81);
INSERT INTO user_post VALUES('turbo', 82);
INSERT INTO user_post VALUES('turbo', 83);
INSERT INTO user_post VALUES('turbo', 84);
INSERT INTO user_post VALUES('turbo', 85);
INSERT INTO user_post VALUES('turbo', 86);
INSERT INTO user_post VALUES('turbo', 87);
INSERT INTO user_post VALUES('turbo', 88);
INSERT INTO user_post VALUES('turbo', 89);
INSERT INTO user_post VALUES('turbo', 90);
INSERT INTO "user_post" VALUES('enzbang', 91);
INSERT INTO "user_post" VALUES('enzbang', 92);
INSERT INTO "user_post" VALUES('enzbang', 93);
INSERT INTO "user_post" VALUES('enzbang', 94);
INSERT INTO "user_post" VALUES('enzbang', 95);
INSERT INTO "user_post" VALUES('enzbang', 96);
INSERT INTO "user_post" VALUES('enzbang', 97);
INSERT INTO "user_post" VALUES('enzbang', 98);
INSERT INTO "user_post" VALUES('enzbang', 99);
INSERT INTO "user_post" VALUES('enzbang', 100);
INSERT INTO "user_post" VALUES('enzbang', 101);
INSERT INTO "user_post" VALUES('enzbang', 102);
INSERT INTO "user_post" VALUES('enzbang', 103);
INSERT INTO "user_post" VALUES('enzbang', 104);
INSERT INTO "user_post" VALUES('enzbang', 105);
INSERT INTO "user_post" VALUES('enzbang', 106);
INSERT INTO "user_post" VALUES('enzbang', 107);
INSERT INTO "user_post" VALUES('enzbang', 108);
INSERT INTO "user_post" VALUES('enzbang', 109);
INSERT INTO "user_post" VALUES('enzbang', 110);
INSERT INTO "user_post" VALUES('enzbang', 111);
INSERT INTO "user_post" VALUES('enzbang', 112);
INSERT INTO "user_post" VALUES('enzbang', 113);
INSERT INTO "user_post" VALUES('enzbang', 114);
INSERT INTO "user_post" VALUES('enzbang', 115);
INSERT INTO "user_post" VALUES('enzbang', 116);
INSERT INTO "user_post" VALUES('enzbang', 117);
INSERT INTO "user_post" VALUES('enzbang', 118);
INSERT INTO "user_post" VALUES('enzbang', 119);
INSERT INTO "user_post" VALUES('enzbang', 120);
INSERT INTO "user_post" VALUES('enzbang', 121);
INSERT INTO "user_post" VALUES('enzbang', 122);
INSERT INTO "user_post" VALUES('enzbang', 123);
INSERT INTO "user_post" VALUES('enzbang', 124);
INSERT INTO "user_post" VALUES('enzbang', 125);
INSERT INTO "user_post" VALUES('enzbang', 126);
INSERT INTO "user_post" VALUES('enzbang', 127);
INSERT INTO "user_post" VALUES('enzbang', 128);
INSERT INTO "user_post" VALUES('enzbang', 129);
INSERT INTO "user_post" VALUES('enzbang', 130);
INSERT INTO "user_post" VALUES('enzbang', 131);
INSERT INTO "user_post" VALUES('enzbang', 132);
INSERT INTO "user_post" VALUES('enzbang', 133);
INSERT INTO "user_post" VALUES('enzbang', 134);
INSERT INTO "user_post" VALUES('enzbang', 135);
INSERT INTO "user_post" VALUES('enzbang', 136);
INSERT INTO "user_post" VALUES('enzbang', 137);
INSERT INTO "user_post" VALUES('enzbang', 138);
INSERT INTO "user_post" VALUES('enzbang', 139);
INSERT INTO "user_post" VALUES('enzbang', 140);
INSERT INTO "user_post" VALUES('enzbang', 141);

--  Photos

INSERT INTO photo
   VALUES(1, '2007/Forum photographies/Paysage/200702011055-0.1.jpg', 800, 532, 82558);
INSERT INTO photo
   VALUES(2, '2007/Forum photographies/Paysage/200702011056-1.1x2.jpg', 800, 532, 38172);
INSERT INTO photo
   VALUES(3, '2007/Forum photographies/Portrait/200702011057-2.5.jpg', 800, 595, 41123);
INSERT INTO photo
   VALUES(4, '2007/Forum photographies/Paysage/200702011102-3.6.jpg', 800, 600, 83107);
INSERT INTO photo
   VALUES(5, '2007/Forum photographies/Paysage/200702011103-4.8.jpg', 800, 532, 52925);
INSERT INTO photo
   VALUES(6, '2007/Forum photographies/Paysage/200702011103-5.9.jpg', 800, 600, 94582);
INSERT INTO photo
   VALUES(7, '2007/Forum photographies/Nature morte/200702011104-6.9x.jpg', 800, 600, 28457);
INSERT INTO photo
   VALUES(8, '2007/Forum photographies/Nature morte/200702011105-7.11.jpg', 800, 533, 29558);
INSERT INTO photo
   VALUES(9, '2007/Forum photographies/Nature morte/200702011106-8.12.jpg', 764, 600, 50060);
INSERT INTO photo
   VALUES(10, '2007/Forum photographies/Macro/Animaux/200702011203-9.15.jpg', 800, 600, 134700);
INSERT INTO photo
   VALUES(11, '2007/Forum photographies/Nature morte/200702011208-10.23.jpg', 800, 531, 58024);
INSERT INTO photo
   VALUES(12, '2007/Forum photographies/Nature morte/200702011209-11.25.jpg', 800, 466, 37296);
INSERT INTO photo
   VALUES(13, '2007/Forum photographies/Abstrait/200702011210-12.25x.jpg', 800, 549, 93859);
INSERT INTO photo
   VALUES(14, '2007/Forum photographies/Abstrait/200702011211-13.29.jpg', 800, 600, 42118);
INSERT INTO photo
   VALUES(15, '2007/Forum photographies/Paysage/200702011212-14.31.jpg', 800, 593, 73722);
INSERT INTO photo
   VALUES(16, '2007/Forum photographies/Abstrait/200702011213-15.38.jpg', 800, 600, 43389);
INSERT INTO photo
   VALUES(17, '2007/Forum photographies/Nature morte/200702011214-16.48.jpg', 800, 532, 52693);
INSERT INTO photo
   VALUES(18, '2007/Forum photographies/Nature morte/200702011214-17.52.jpg', 800, 600, 43914);
INSERT INTO photo
   VALUES(19, '2007/Forum photographies/Nature morte/200702011215-18.53.jpg', 800, 600, 55107);
INSERT INTO photo
   VALUES(20, '2007/Forum photographies/Paysage/200702011216-19.55.jpg', 800, 600, 33503);
INSERT INTO photo
   VALUES(21, '2007/Forum photographies/Nature morte/200702011216-20.58.jpg', 800, 558, 54012);
INSERT INTO photo
   VALUES(22, '2007/Forum photographies/Nature morte/200702011217-21.61.jpg', 600, 800, 43378);
INSERT INTO photo
   VALUES(23, '2007/Forum photographies/Paysage/200702011217-22.71.jpg', 600, 800, 143269);
INSERT INTO photo
   VALUES(24, '2007/Forum photographies/Nature morte/200702011218-23.73.jpg', 600, 600, 14464);
INSERT INTO photo
   VALUES(25, '2007/Forum photographies/Paysage/200702011219-24.91.jpg', 800, 600, 78384);
INSERT INTO photo
   VALUES(26, '2007/Forum photographies/Macro/Animaux/200702011219-25.106.jpg', 788, 600, 54892);
INSERT INTO photo
   VALUES(27, '2007/Forum photographies/Abstrait/200702011220-26.110.jpg', 800, 600, 97441);
INSERT INTO photo
   VALUES(28, '2007/Forum photographies/Nature morte/200702011221-27.AGRIC055.jpg', 640, 800, 87672);
INSERT INTO photo
   VALUES(29, '2007/Forum photographies/Macro/Animaux/200702011222-28.ANIMAUX089.jpg', 750, 600, 39951);
INSERT INTO photo
   VALUES(30, '2007/Forum photographies/Macro/Animaux/200702011222-29.ANIMAUX113.jpg', 750, 600, 73108);
INSERT INTO photo
   VALUES(31, '2007/Forum photographies/Macro/Animaux/200702011223-30.ANIMAUX114.jpg', 750, 600, 71637);
INSERT INTO photo
   VALUES(32, '2007/Forum photographies/Paysage/200702011223-31.ARCHIT001.jpg', 750, 600, 89270);
INSERT INTO photo
   VALUES(33, '2007/Forum photographies/Paysage/200702011224-32.ARCHT072.jpg', 600, 750, 25293);
INSERT INTO photo
   VALUES(34, '2007/Forum photographies/Abstrait/200702011225-33.FONDS009.jpg', 750, 600, 90927);
INSERT INTO photo
   VALUES(35, '2007/Forum photographies/Paysage/200702011226-34.PERS008.jpg', 750, 600, 59954);
INSERT INTO photo
   VALUES(36, '2007/Forum photographies/Portrait/200702011227-35.PERS012.jpg', 750, 600, 103911);
INSERT INTO photo
   VALUES(37, '2007/Forum photographies/Portrait/200702011227-36.PERS024.jpg', 750, 600, 72551);
INSERT INTO "photo" VALUES(38, '2007/Forum photographies/Paysage/200702012114-0.conv_1.jpg', 800, 600, 72762);
INSERT INTO "photo" VALUES(39, '2007/Forum photographies/Paysage/200702012140-1.conv_4.jpg', 800, 600, 146591);
INSERT INTO "photo" VALUES(40, '2007/Forum photographies/Macro/Animaux/200702012140-2.conv_8.jpg', 800, 533, 37338);
INSERT INTO "photo" VALUES(41, '2007/Forum photographies/Paysage/200702012141-3.conv_10.jpg', 800, 533, 147253);
INSERT INTO "photo" VALUES(42, '2007/Forum photographies/Paysage/200702012141-4.conv_10b.jpg', 800, 531, 113581);
INSERT INTO "photo" VALUES(43, '2007/Forum photographies/Abstrait/200702012142-5.conv_11.jpg', 532, 800, 56176);
INSERT INTO "photo" VALUES(44, '2007/Forum photographies/Abstrait/200702012143-6.conv_11b.jpg', 800, 600, 65582);
INSERT INTO "photo" VALUES(45, '2007/Forum photographies/Macro/Animaux/200702012143-7.conv_12.jpg', 800, 599, 77307);
INSERT INTO "photo" VALUES(46, '2007/Forum photographies/Macro/Animaux/200702012144-8.conv_12b.jpg', 800, 600, 123416);
INSERT INTO "photo" VALUES(47, '2007/Forum photographies/Abstrait/200702012144-9.conv_13.jpg', 800, 600, 94453);
INSERT INTO "photo" VALUES(48, '2007/Forum photographies/Macro/Animaux/200702012145-10.conv_17.jpg', 800, 600, 112182);
INSERT INTO "photo" VALUES(49, '2007/Forum photographies/Paysage/200702012145-11.conv_19.jpg', 800, 533, 85781);
INSERT INTO "photo" VALUES(50, '2007/Forum photographies/Macro/Animaux/200702012146-12.conv_25.jpg', 800, 600, 137316);
INSERT INTO "photo" VALUES(51, '2007/Forum photographies/Macro/Animaux/200702012146-13.conv_25b.jpg', 800, 533, 51365);
INSERT INTO "photo" VALUES(52, '2007/Forum photographies/Paysage/200702012147-14.conv_30.jpg', 533, 800, 60825);
INSERT INTO "photo" VALUES(53, '2007/Forum photographies/Paysage/200702012147-15.conv_31.jpg', 800, 533, 95442);
INSERT INTO "photo" VALUES(54, '2007/Forum photographies/Paysage/200702012148-16.conv_32.jpg', 800, 600, 99593);
INSERT INTO "photo" VALUES(55, '2007/Forum photographies/Abstrait/200702012148-17.conv_34.jpg', 800, 524, 79084);
INSERT INTO "photo" VALUES(56, '2007/Forum photographies/Macro/Animaux/200702012149-18.conv_36.jpg', 800, 600, 20425);
INSERT INTO "photo" VALUES(57, '2007/Forum photographies/Paysage/200702012149-19.conv_39.jpg', 800, 533, 149062);
INSERT INTO "photo" VALUES(58, '2007/Forum photographies/Paysage/200702012149-20.conv_40.jpg', 800, 600, 100181);
INSERT INTO "photo" VALUES(59, '2007/Forum photographies/Paysage/200702012150-21.conv_40b.jpg', 800, 657, 102548);
INSERT INTO "photo" VALUES(60, '2007/Forum photographies/Paysage/200702012150-22.conv_42.jpg', 533, 800, 123293);
INSERT INTO "photo" VALUES(61, '2007/Forum photographies/Abstrait/200702012151-23.conv_43.jpg', 600, 800, 141948);
INSERT INTO "photo" VALUES(62, '2007/Forum photographies/Abstrait/200702012151-24.conv_46.jpg', 800, 600, 74131);
INSERT INTO "photo" VALUES(63, '2007/Forum photographies/Abstrait/200702012151-25.conv_49.jpg', 800, 600, 39485);
INSERT INTO "photo" VALUES(64, '2007/Forum photographies/Abstrait/200702012152-26.conv_49b.jpg', 800, 532, 105675);
INSERT INTO "photo" VALUES(65, '2007/Forum photographies/Paysage/200702012152-27.conv_55.jpg', 800, 600, 123954);
INSERT INTO "photo" VALUES(66, '2007/Forum photographies/Abstrait/200702012153-28.conv_57.jpg', 800, 532, 87446);
INSERT INTO "photo" VALUES(67, '2007/Forum photographies/Nature morte/200702012153-29.conv_58.jpg', 800, 500, 85840);
INSERT INTO "photo" VALUES(68, '2007/Forum photographies/Paysage/200702012154-30.conv_60.jpg', 533, 800, 57675);
INSERT INTO "photo" VALUES(69, '2007/Forum photographies/Abstrait/200702012155-31.conv_63.jpg', 800, 600, 61868);
INSERT INTO "photo" VALUES(70, '2007/Forum photographies/Macro/Animaux/200702012155-32.conv_67.jpg', 800, 600, 87149);
INSERT INTO "photo" VALUES(71, '2007/Forum photographies/Portrait/200702012156-33.conv_70.jpg', 531, 800, 47871);
INSERT INTO "photo" VALUES(72, '2007/Forum photographies/Paysage/200702012156-34.conv_71.jpg', 800, 533, 192053);
INSERT INTO "photo" VALUES(73, '2007/Forum photographies/Macro/Animaux/200702012157-35.conv_80.jpg', 800, 523, 35783);
INSERT INTO "photo" VALUES(74, '2007/Forum photographies/Nature morte/200702012157-36.conv_92.jpg', 800, 600, 180136);
INSERT INTO "photo" VALUES(75, '2007/Forum photographies/Macro/Animaux/200702012157-37.conv_93.jpg', 800, 587, 138881);
INSERT INTO "photo" VALUES(76, '2007/Forum photographies/Abstrait/200702012158-38.conv_96.jpg', 800, 600, 188896);
INSERT INTO "photo" VALUES(77, '2007/Forum photographies/Paysage/200702012158-39.conv_99.jpg', 800, 532, 117796);
INSERT INTO "photo" VALUES(78, '2007/Forum photographies/Paysage/200702012159-40.conv_105.jpg', 532, 800, 88058);
INSERT INTO "photo" VALUES(79, '2007/Forum photographies/Macro/Animaux/200702012159-41.conv_ANIMX031.jpg', 800, 526, 37963);
INSERT INTO "photo" VALUES(80, '2007/Forum photographies/Paysage/200702012159-42.conv_ARBRE017.jpg', 800, 640, 34189);
INSERT INTO "photo" VALUES(81, '2007/Forum photographies/Portrait/200702012200-43.conv_ARBRE023.jpg', 800, 640, 66731);
INSERT INTO "photo" VALUES(82, '2007/Forum photographies/Paysage/200702012200-44.conv_COTES064.jpg', 800, 529, 49048);
INSERT INTO "photo" VALUES(83, '2007/Forum photographies/Abstrait/200702012201-45.conv_MAISN002.jpg', 800, 543, 22580);
INSERT INTO "photo" VALUES(84, '2007/Forum photographies/Macro/Animaux/200702012201-46.conv_MONUM113.jpg', 800, 527, 49269);
INSERT INTO "photo" VALUES(85, '2007/Forum photographies/Paysage/200702012202-47.conv_PARIS008.jpg', 506, 800, 30177);
INSERT INTO "photo" VALUES(86, '2007/Forum photographies/Paysage/200702012202-48.conv_SPTLS046.jpg', 529, 800, 49362);
INSERT INTO "photo" VALUES(87, '2007/Forum photographies/Nature morte/200702012202-49.conv_TEX009.jpg', 800, 533, 78529);
INSERT INTO "photo" VALUES(88, '2007/Forum photographies/Paysage/200702012203-50.conv_TRSPT066.jpg', 641, 800, 54089);


--  Comments

INSERT INTO "comment" VALUES(1, '2007-02-01 19:29:21', NULL, 'enzbang', NULL, 'See <a href=''http://en.wikipedia.org/wiki/Rubik%27s_cube'' rel=''nofollow''>http://en.wikipedia.org/wiki/Rubik%27s_cube</a> if you''re interested...', NULL);
INSERT INTO "comment" VALUES(2, '2007-02-01 19:30:22', NULL, 'enzbang', NULL, 'Quel est cet animal ?', NULL);
INSERT INTO "comment" VALUES(3, '2007-02-01 19:33:57', NULL, 'enzbang', NULL, 'Parque Nacional de Monegros ?', NULL);
INSERT INTO "comment" VALUES(4, '2007-02-01 19:34:19', NULL, 'enzbang', NULL, 'Filtre polarisant ?', NULL);
INSERT INTO "comment" VALUES(5, '2007-02-01 19:34:54', NULL, 'enzbang', NULL, 'Id&#195;&#169;e originale ! <em>bravo</em> ', NULL);
INSERT INTO "comment" VALUES(6, '2007-02-01 19:35:45', NULL, 'enzbang', NULL, 'Tiens les accents ne passent pas ? &#195;&#169;&#195;&#160;&#195;&#185;&#195;&#179;', NULL);
INSERT INTO "comment" VALUES(7, '2007-02-01 19:36:06', NULL, 'enzbang', NULL, 'Hum... sans moi', NULL);
INSERT INTO "comment" VALUES(8, '2007-02-01 19:36:59', NULL, 'enzbang', NULL, '&#195;&#167;a penche !', NULL);
INSERT INTO "comment" VALUES(9, '2007-02-01 19:38:19', NULL, 'enzbang', NULL, 'la m&#195;&#170;me avec un cadre ?', NULL);
INSERT INTO "comment" VALUES(10, '2007-02-01 19:39:02', NULL, 'enzbang', NULL, 'J''aurais cadr&#195;&#169; l&#195;&#169;g&#195;&#168;rement plus &#195;&#160; droite', NULL);
INSERT INTO "comment" VALUES(11, '2007-02-01 19:39:31', NULL, 'enzbang', NULL, 'D&#195;&#169;sol&#195;&#169; mais je n''accroche pas', NULL);
INSERT INTO "comment" VALUES(12, '2007-02-01 19:40:30', NULL, 'enzbang', NULL, 'Good idea 4 this picture', NULL);
INSERT INTO "comment" VALUES(13, '2007-02-01 19:40:54', NULL, 'enzbang', NULL, 'Un classique', NULL);
INSERT INTO "comment" VALUES(14, '2007-02-01 19:41:28', NULL, 'enzbang', NULL, '^_^', NULL);
INSERT INTO "comment" VALUES(15, '2007-02-01 19:41:50', NULL, 'enzbang', NULL, 'Unlocked ?', NULL);
INSERT INTO "comment" VALUES(16, '2007-02-01 19:42:27', NULL, 'enzbang', NULL, 'L''envie de fraise d&#195;&#169;j&#195;&#160; !', NULL);

INSERT INTO "post_comment" VALUES(60, 1);
INSERT INTO "post_comment" VALUES(84, 2);
INSERT INTO "post_comment" VALUES(87, 3);
INSERT INTO "post_comment" VALUES(54, 4);
INSERT INTO "post_comment" VALUES(64, 5);
INSERT INTO "post_comment" VALUES(64, 6);
INSERT INTO "post_comment" VALUES(68, 7);
INSERT INTO "post_comment" VALUES(76, 8);
INSERT INTO "post_comment" VALUES(77, 9);
INSERT INTO "post_comment" VALUES(78, 10);
INSERT INTO "post_comment" VALUES(81, 11);
INSERT INTO "post_comment" VALUES(69, 12);
INSERT INTO "post_comment" VALUES(70, 13);
INSERT INTO "post_comment" VALUES(71, 14);
INSERT INTO "post_comment" VALUES(72, 15);
INSERT INTO "post_comment" VALUES(56, 16);
