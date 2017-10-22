import React from 'react';
import { Container, Row, Col } from 'reactstrap';
import { truncate } from '../../utils';

const CulturalHeritage = ({ culturalHeritage }) => (
  <Container className="cultural-heritage">
    <Row>
      <Col xs="3">
        { culturalHeritage.images.length > 0 ? (
          <img alt="Cultural Heritage" src={ culturalHeritage.images[0].url } />
          ) : (
          <span>No Image</span>
          )
        }
      </Col>
      <Col xs="9">
        <h2>{ culturalHeritage.title }</h2>
        <hr />
        <p>{ truncate(culturalHeritage.description) }</p> 
      </Col>
    </Row>
  </Container>
)

export default CulturalHeritage;
