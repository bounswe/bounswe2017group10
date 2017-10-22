import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';

const Page = ({ user, token, culturalHeritages, loadCulturalHeritages }) => (
  <Container>
    <Row>
      <Col xs="9">
        <NavLink to="/cultural-heritages/new">New Cultural Heritage</NavLink>
        <ul className="cultural-heritages">
          { culturalHeritages && culturalHeritages.map(c => (
            <NavLink key={ c.id } to={ "/cultural-heritages/" + c.id }>
              <li className="cultural-heritage">
                <CulturalHeritage culturalHeritage={ c } shouldTruncate ={ true }/>
              </li>
            </NavLink>
          ))} 
        </ul>
      </Col>
    </Row>
  </Container>
)

export default Page;

