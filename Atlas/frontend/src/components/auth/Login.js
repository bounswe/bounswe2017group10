import React, { Component } from 'react';
import { Button, Form, FormGroup, Input, Col, Row, Container, Badge } from 'reactstrap';
import './style.css';

export default class Login extends Component {

    constructor(props){
        super(props);
        this.state = {username: '', password: ''};

        this.handleInputChange = this.handleInputChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    handleSubmit(){
        //request server and redirect to main page or fail to log in
      this.props.saveToken("wowtoken");
    }

    handleInputChange(event){
      const target = event.target;
      const name = target.name;
      this.setState({
        [name]: target.value
      });
    }

    render() {
        return (
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
                        value={this.state.username}
                        onChange={this.handleInputChange}
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
                        value={this.state.password}
                        onChange={this.handleInputChange}
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
                    <Button onClick={ this.handleSubmit } style={{ float: 'right' }}>Log-in</Button>
                </Col>

            </Row>
          </Form>
        </Container>
        );
    }
};

