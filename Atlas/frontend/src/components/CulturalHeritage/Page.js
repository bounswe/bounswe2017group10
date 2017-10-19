import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Button, Form, FormGroup, Input, Col, Row, Container, Badge, Alert } from 'reactstrap';

const Page = ({ user, token, culturalHeritages, loadCulturalHeritages }) => ({
  componentDidMount() {
    // loadCulturalHeritages(token);
  },

  render() {
    return (
      <Container>
        <Row>
          <Col xs="9">
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
  }
})

export default Page;

