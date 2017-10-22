import React from 'react';
import { Container, Row, Col } from 'reactstrap';
import { truncate } from '../../utils';
import HomeIcon from 'react-icons/lib/fa/home';
import BankIcon from 'react-icons/lib/fa/bank';

const CulturalHeritage = ({ culturalHeritage, shouldTruncate = false }) => (
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
        <p>{ shouldTruncate
          ? truncate(culturalHeritage.description)
          : culturalHeritage.description
          }
        </p> 
        <hr />
        { culturalHeritage.continent &&
          <label className="small-label">
            <BankIcon />
            <span> { culturalHeritage.continent }</span>
          </label>
        }
        { culturalHeritage.country &&
          <label className="small-label">
            <HomeIcon />
            <span> { culturalHeritage.country }</span>
          </label>
        }
      </Col>
    </Row>
  </Container>
)

export default CulturalHeritage;
