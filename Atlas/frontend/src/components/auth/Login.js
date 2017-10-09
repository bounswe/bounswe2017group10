import React, { Component } from 'react';
import {Button, Form, FormGroup, Input, Col, Row, Container, Badge} from 'reactstrap';
import './style.css';

export default class Login extends Component {

    constructor(props){
        super(props);
        this.state = {username: '', password: ''};

        this.handleNameChange = this.handleNameChange.bind(this);
        this.handlePasswordChange = this.handlePasswordChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }


    handleSubmit(){
        //request server and redirect to main page or fail to log in
    }

    handleNameChange(event){
        this.setState({username: event.target.username});
    }

    handlePasswordChange(event){
        this.setState({password: event.target.password});
    }

    handleResponse = (data) => {
        console.log(data);
    }

    handleError = (error) => {
        this.setState({ error });
    }


    render() {
        return (
        <Container>
          <Form className="login-panel" onSubmit={this.handleSubmit}>
            <Row>
                <h3>Log-in</h3>
            </Row>
            <hr/>
            <FormGroup>
                <Row>
                    <Col xs="2">
                        <label>
                            Username:
                        </label>
                    </Col>
                    <Col xs="9">
                        <Input className="username-input" type="text" value={this.state.username} onChange={this.handleNameChange}/>
                    </Col>
                </Row>
            </FormGroup>
            <FormGroup>
                <Row>
                    <Col xs="2">
                        <label>
                            Password:
                        </label>
                    </Col>
                    <Col xs="9">
                        <Input className="password-input" type="text" value={this.state.password} onChange={this.handlePasswordChange}/>
                    </Col>
                </Row>
            </FormGroup>
            <Row>
                <Col xs="7"></Col>
                <Col xs="5">
                    <a href="">Forgot my password</a><br/>
                </Col>
            </Row>
            <Row>
                <Col xs="6">
                    <Button className="facebook-button"><Badge >f</Badge>Login with Facebook</Button>
                </Col>
                <Col xs="5">
                    <Button onClick={ this.handleSubmit } className="login-button">Log-in</Button>
                </Col>

            </Row>
          </Form>
        </Container>
        );
    }
};

