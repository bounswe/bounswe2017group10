import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import LeftIcon from 'react-icons/lib/fa/angle-left';

const Show = ({ user, token, culturalHeritage }) => (
  <Container>
    <NavLink className="atlas-button" to="/cultural-heritages">
      <LeftIcon />
      Back
    </NavLink>
    <Row>
      <Col xs="12">
        <CulturalHeritage culturalHeritage={ culturalHeritage } shouldTruncate ={ false }/>
      </Col>
    </Row>
  </Container>
)

export default Show;
