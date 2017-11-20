import React from 'react';
import { Container, Row, Col } from 'reactstrap';
import { truncate } from '../../utils';
import HomeIcon from 'react-icons/lib/fa/home';
import BankIcon from 'react-icons/lib/fa/bank';
import TagIcon from 'react-icons/lib/fa/tag';
import Comment from './Comment';

const CulturalHeritage = ({ culturalHeritage, shouldTruncate = false, showComments = false }) => (
  <Container>
    <Row className="whitebox cultural-heritage">
      <Col xs="5">
        { culturalHeritage.images.length > 0 ? (
          <img alt="Cultural Heritage" src={ culturalHeritage.images[0].url } />
          ) : (
          <span>No Image</span>
          )
        }
      </Col>
      <Col xs="7">
        <h2>{ culturalHeritage.title }</h2>
        <hr />
        <p>{ shouldTruncate
          ? truncate(culturalHeritage.description)
          : culturalHeritage.description
          }
        </p> 
        <hr />
        { culturalHeritage.country &&
          <label className="small-label">
            <BankIcon />
            <span> { culturalHeritage.country }</span>
          </label>
        }
        { culturalHeritage.city &&
          <label className="small-label">
            <HomeIcon />
            <span> { culturalHeritage.city }</span>
          </label>
        }
        { culturalHeritage.tags.map(tag => (
          <label className="small-label" key={ tag['id'] }>
            <TagIcon />
            <span>{ tag['name'] }</span>
          </label>
        ))}
      </Col>
    </Row>
    { showComments && culturalHeritage.comments.map(comment => (
      <Row key={ Math.random() } className="whitebox" style={ { marginTop: 20 } }>
        <Col xs="12">
            <Comment comment={ comment } />
        </Col>
      </Row>
    ))}
  </Container>
)

export default CulturalHeritage;
