import React from 'react';
import './style.css';
import { Modal, Button, Form, FormGroup, Input, Col, Row, Container, Badge, Alert } from 'reactstrap';
import { NavLink } from 'react-router-dom';

const Page = ({ user, token, addCHInputs, handleCHInputChange, createCH }) => (
  <Container>
    <NavLink to="/cultural-heritages">Back</NavLink>
    <h1 style={{ textAlign: 'center' }}>Add Cultural Heritage</h1> 
    <hr />
    <Form>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Title:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="title"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Description:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="description"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
        <Button onClick={ () => createCH(addCHInputs, token) }>Add Cultural Heritage</Button>
      </FormGroup>
    </Form>
  </Container>
)

export default Page;
