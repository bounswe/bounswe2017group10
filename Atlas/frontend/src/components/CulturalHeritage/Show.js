import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { NavLink } from 'react-router-dom';
import LeftIcon from 'react-icons/lib/fa/angle-left';
import { Button, Form, FormGroup, Input, Col, Row, Container } from 'reactstrap';
import { getUrlParam } from '../../utils';
import RecommendedItem from './RecommendedItem';
import { RECOMMENDATION_LIMIT } from '../../constants';

const Show = ({ user, token, culturalHeritage, commentInput, recommendations, recommendationLoadCompleted, commentInputChange, postComment, favoriteItem, removeClick, annotations, showAnnotation, hideAnnotation, annotationInput, updateAnnotationInput, createAnnotation, openAnnotationInput, closeAnnotationInput }) => (
  culturalHeritage !== undefined &&
    <Container>
      <Row>
        <Col xs="3">
          <NavLink className="atlas-button" to={ getUrlParam('returnTo') }>
            <LeftIcon />
            Back
          </NavLink>
        </Col>
        <Col xs="6">
            {(culturalHeritage.user == user.id) && <Button style={{ background:'red', float: 'right', fontSize: 18, textAlign: 'right' }} onClick={() => removeClick(token, culturalHeritage.id)}>Remove Item</Button>}
        </Col>
      </Row>


      <Row>
        <Col xs="9">
          <CulturalHeritage culturalHeritage={ culturalHeritage } shouldTruncate={ false } showComments={ true } favorite={ () => favoriteItem(token, culturalHeritage) } withLink={ false } showAnnotations={ true } annotations={ annotations } showAnnotation={ showAnnotation } hideAnnotation={ hideAnnotation } annotationInput={ annotationInput } updateAnnotationInput={ updateAnnotationInput } createAnnotation={ createAnnotation } openAnnotationInput={ openAnnotationInput } closeAnnotationInput={ closeAnnotationInput }/>
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
          <div className="atlas-button" onClick={ () => postComment(token, culturalHeritage.id, commentInput) }>Post Comment</div>
        </Col>
        <Col xs="3" className="recommended-items">
          <Row>
            <Col xs="12">
              <h1>Recommended For You</h1>
            </Col>
          </Row>
          { recommendationLoadCompleted
            ? recommendations.slice(0, RECOMMENDATION_LIMIT).map(r => (
                <RecommendedItem key={ r.id } culturalHeritage={ r } />
              ))
            : <div>Loading...</div>
          }
        </Col>
      </Row>
    </Container>
)

export default Show;
