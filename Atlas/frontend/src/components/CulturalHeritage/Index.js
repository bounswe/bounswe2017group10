import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Modal, Button, Form, FormGroup, Input, Col, Row, Container, Badge, Alert } from 'reactstrap';
import { NavLink } from 'react-router-dom';

const Page = ({ user, token, culturalHeritages, loadCulturalHeritages }) => (
  <Container>
    <Row>
      <Col xs="9">
        <NavLink to="/cultural-heritages/new">New Cultural Heritage</NavLink>
        <ul className="cultural-heritages">
          { culturalHeritages.map(c => (
            <li key={ c.id } className="cultural-heritage-listitem">
              <CulturalHeritage culturalHeritage={ c }/>
            </li>
          ))} 
        </ul>
      </Col>
    </Row>
  </Container>
)

export default Page;

