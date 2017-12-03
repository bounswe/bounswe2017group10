import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { NavLink } from 'react-router-dom';
import LeftIcon from 'react-icons/lib/fa/angle-left';
import { Button, Form, FormGroup, Input, Col, Row, Container } from 'reactstrap';


const Show = ({ user, token, culturalHeritage, commentInput, commentInputChange, postComment, favoriteItem }) => (culturalHeritage !== undefined &&

  <Container>
    <NavLink className="atlas-button" to="/cultural-heritages">
      <LeftIcon />
      Back
    </NavLink>
    <Row>
      <Col xs="12">
        <CulturalHeritage culturalHeritage={ culturalHeritage } shouldTruncate={ false } showComments={ true } favorite={ () => favoriteItem(token, culturalHeritage) }/>
      </Col>
    </Row>
    <Row>
      <Col xs="12">
        <FormGroup style={{ marginTop: 30 }}>
          <Form>
              <Input
                name="username_or_email"
                type="textarea"
                onChange={commentInputChange}
                value={ commentInput }
                className="comment-input"
                placeholder="Add a comment to this cultural heritage"
              />
          </Form>
        </FormGroup>
      </Col>
    </Row>
    <Row>
        <Col xs="12">
            <div className="atlas-button" onClick={ () => postComment(token, culturalHeritage.id, commentInput) }>Post Comment</div>
        </Col>
    </Row>
  </Container>
)

export default Show;
