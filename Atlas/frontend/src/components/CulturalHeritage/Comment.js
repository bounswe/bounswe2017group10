import React from 'react';
import ClockIcon from 'react-icons/lib/fa/clock-o';
import Moment from 'react-moment'

const Comment = ({ comment }) => (
  <div>
    {comment.text}
    <hr />
    <span className="date">
        <ClockIcon /> <Moment fromNow parse="YYYY-MM-DDTHH:mm:ssZ">{ comment.created_time } </Moment>
    </span>
  </div>
)

export default Comment;
