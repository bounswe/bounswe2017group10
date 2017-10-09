import React from 'react';
import { Button, Form, FormGroup, Input, Col, Row, Container, Badge } from 'reactstrap';
import './style.css';

const Signup = ({ inputChanged, attemptSignup }) => (
  <Container className="square-box">
    <Form className="signup-panel"  onSubmit={attemptSignup}>
      <h3>Sign-up</h3>
      <hr/>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      Email:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="email" type="text" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      Your Name:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="name" type="text" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      Your Surname:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="surname" type="text" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      User name:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="username" type="text" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      Password:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="password" type="password" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>
                      Confirm password:
                  </label>
              </Col>
              <Col xs="9">
                  <Input name="confirm_password" type="password" onChange={inputChanged}/>
              </Col>
          </Row>
      </FormGroup>
      <Row>
          <Col xs="12">
              <Button className="facebook-button"><Badge >f</Badge> Sign-up with Facebook</Button>
              <Button onClick={ attemptSignup } style={{ float: 'right' }}>Sign-up</Button>
          </Col>
      </Row>
    </Form>
  </Container>
)

export default Signup;
