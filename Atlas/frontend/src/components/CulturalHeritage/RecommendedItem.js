import React from 'react';
import { Container, Row, Col } from 'reactstrap';
import { truncate } from '../../utils';
import HomeIcon from 'react-icons/lib/fa/home';
import BankIcon from 'react-icons/lib/fa/bank';
import TagIcon from 'react-icons/lib/fa/tag';
import Comment from './Comment';
import AtlasHeader from '../utils/AtlasHeader'
import CHFav from './CHFav';
import { NavLink } from 'react-router-dom';
import { loadSingleItem } from '../../actions/culturalHeritage';

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
