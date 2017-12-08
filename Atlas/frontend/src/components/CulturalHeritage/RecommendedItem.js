import React from 'react';
import { Row, Col } from 'reactstrap';

const CulturalHeritage = ({ culturalHeritage }) => {
  return (
    <a href={ "/cultural-heritages/" + culturalHeritage.id } className="recommended-item" >
      <Row>
        <Col xs="12">
          { culturalHeritage.images.length > 0 ? (
            <img className="ch-image" alt="Cultural Heritage" src={ culturalHeritage.images[0].url } />
            ) : (
            <span>No Image</span>
            )
          }
        </Col>
      </Row>
      <Row>
        <Col xs="12">
          <h2>{ culturalHeritage.title }</h2>
        </Col>
      </Row>
    </a>
  )
}

export default CulturalHeritage;
