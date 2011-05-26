alter table user_preferences add column "start_comment_visible" boolean default TRUE;

update user_preferences
  set start_comment_visible='TRUE';
