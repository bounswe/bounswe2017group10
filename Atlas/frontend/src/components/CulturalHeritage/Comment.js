import React from 'react';
import ClockIcon from 'react-icons/lib/fa/clock-o';

const Comment = ({ comment }) => (
  <div>
    {comment.text}
    <hr />
    <span className="date">
      <ClockIcon /> { comment.created_time }
    </span>
  </div>
)

export default Comment;
