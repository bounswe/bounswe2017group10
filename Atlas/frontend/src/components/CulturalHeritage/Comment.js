import React from 'react';
import ClockIcon from 'react-icons/lib/fa/clock-o';
import Moment from 'react-moment'
import { Row, Col } from 'reactstrap';
import unknown from '../../assets/images/unknown.png'

const Comment = ({ comment }) => (
  <Row className="whitebox-comment">
      <Col xs="2" className="info">
          { comment.user_info.picture == null ? (
            // eslint-disable-next-line
              <img alt="Profile Picture" src={ unknown } />
          ) : (
            // eslint-disable-next-line
              <img alt="Profile Picture" src={ comment.user_info.picture } />
          )
          }
          <h3>{comment.user_info.username}</h3>
      </Col>
      <Col xs="10">
          {comment.text}
          <hr />
    <span className="date">
        <ClockIcon /> <Moment fromNow parse="YYYY-MM-DDTHH:mm:ssZ">{ comment.created_time } </Moment>
    </span>
      </Col>
  </Row>
)

export default Comment;
