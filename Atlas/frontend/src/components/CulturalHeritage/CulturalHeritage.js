import React from 'react';
import { Image, Container, Row, Col } from 'reactstrap';

const CulturalHeritage = ({ culturalHeritage }) => (
  <Container className="cultural-heritage">
    <Row>
      <Col xs="3">
        <img alt="Cultural Heritage Image" src={ culturalHeritage.image } />
      </Col>
      <Col xs="9">
        <h2>{ culturalHeritage.title }</h2>
        <hr />
        <p>{ culturalHeritage.description }</p> 
      </Col>
    </Row>
  </Container>
)

export default CulturalHeritage;
