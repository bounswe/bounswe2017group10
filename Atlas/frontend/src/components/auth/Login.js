import React from 'react';
import { Button, Form, FormGroup, Input, Col, Row, Container, Badge, Alert } from 'reactstrap';
import './style.css';
import { withRouter } from 'react-router-dom';

const Login = ({ history, token, username, password, loginError, attemptLogin, handleInputChange }) => (
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
                  type="password"
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
              <Button onClick={ () => { attemptLogin(username, password, () => history.push({pathname: '/profile', state: { from: 'Main' }})) } } style={{ float: 'right' }}>Log-in</Button>
          </Col>
      </Row>
      <Row className="errors">
        <Col xs="12">
          { loginError === null ? '' : <Alert color="danger">Login failed.</Alert> }
        </Col>
      </Row>
    </Form>
  </Container>
);

export default withRouter(Login);
