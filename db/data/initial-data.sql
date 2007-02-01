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
