--  Create new triggers that will not count has_voted comment into the
--  comment counter.

drop trigger after_post_insert;
drop trigger after_post_comment_insert;

create trigger after_post_insert after insert on post
   begin
      update post
         set last_comment_id=(select max(comment_id) from post_comment, comment
                              where comment_id = comment.id
                              and post_comment.comment_id = comment_id
                              and comment.has_voted = "FALSE")
         where id = new.id;
      update forum
         set last_activity=datetime(current_timestamp)
         where forum.id =
	       (select category.forum_id
	        from category
		where category.id = new.category_id);
   end;

create trigger after_post_comment_insert after insert on post_comment
   begin
      update post
         set comment_counter=comment_counter + 1,
             last_comment_id=new.comment_id
         where id = (select post_id from comment, post_comment
                     where post_comment.comment_id = new.comment_id
                     and comment.id = post_comment.comment_id
                     and comment.has_voted = "FALSE");
      update forum
         set last_activity=datetime(current_timestamp)
         where forum.id =
	       (select category.forum_id
	        from category, post
		where new.post_id = post.id
		  and post.category_id = category.id);
   end;
