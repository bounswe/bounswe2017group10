import React from 'react';
import { Button, Form, FormGroup, Input, Col, Row, Container, Badge } from 'reactstrap';
import './style.css';

const Login = ({ token, username, password, loginError, addToken, attemptLogin, handleInputChange }) => (
  <Container className="square-box">
    <Form onSubmit={this.handleSubmit}>
      <h3>Log-in</h3>
      <hr />
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Username:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="username"
                  type="text"
                  onChange={handleInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Password:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="password"
                  type="text"
                  onChange={handleInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <Row className="small-text">
          <Col xs="12">
              <a href="">Forgot my password</a><br/>
          </Col>
      </Row>
      <Row className="login-buttons">
          <Col xs="12">
              <Button className="facebook-button"><Badge >f</Badge> Login with Facebook</Button>
              <Button onClick={ () => { attemptLogin(username, password) } } style={{ float: 'right' }}>Log-in</Button>
          </Col>
      </Row>
      <span>
        { loginError || '' }
      </span>
    </Form>
  </Container>
);

export default Login;
