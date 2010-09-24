--  Compute and reset the comment counter
--  This was originally designed to remove the vote from the comment counter

update post
   set comment_counter =
      (select count(comment.id) from comment, post_comment
        where post.id=post_comment.post_id
          and post_comment.post_id=post.id
          and comment.id=post_comment.comment_id
          and comment.has_voted="FALSE");
