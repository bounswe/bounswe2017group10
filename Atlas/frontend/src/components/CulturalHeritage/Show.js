import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';

const Show = ({ user, token, culturalHeritage }) => (
  <Container>
    <NavLink to="/cultural-heritages">Back</NavLink>
    <Row>
      <Col xs="12">
        <CulturalHeritage culturalHeritage={ culturalHeritage } shouldTruncate ={ false }/>
      </Col>
    </Row>
  </Container>
)

export default Show;
