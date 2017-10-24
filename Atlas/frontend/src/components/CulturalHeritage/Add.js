import React from 'react';
import './style.css';
import { Button, Form, FormGroup, Input, Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PlusIcon from 'react-icons/lib/fa/plus-circle';

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
                  type="textarea"
                  rows="7"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Image URL:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="img_url"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
        <Button onClick={ () => createCH(addCHInputs, token) }><PlusIcon /> Add Cultural Heritage</Button>
      </FormGroup>
    </Form>
  </Container>
)

export default Page;
