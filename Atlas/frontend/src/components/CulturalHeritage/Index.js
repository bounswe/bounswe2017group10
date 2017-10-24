import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PlusIcon from 'react-icons/lib/fa/plus-circle';

const Page = ({ user, token, culturalHeritages, loadCulturalHeritages }) => (
  <Container>
    <Row>
      <Col xs="9">
        <NavLink className="atlas-button" to="/cultural-heritages/new"><PlusIcon /> New Cultural Heritage</NavLink>
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

