import React, { Component } from 'react';
import {Col, Row, Container} from 'reactstrap';
import Signup from "../../components/auth/Signup";
import Login from "../../components/auth/Login";
import './style.css';

export default class LoginRoute extends Component {

    constructor(props){
        super(props);
        this.state = {username: '', password: ''};
    }

    render() {
        return (
        <Container>
            <Row className="login-container">
                <Col xs="5" className="login-column">
                  <Login saveToken={ this.props.saveToken.bind(this) }/>
                </Col>
                <Col xs="2" /> { /* reactstrap's offsets don't work */ }
                <Col xs="5" className="signup-column">
                  <Signup/>
                </Col>
            </Row>
        </Container>
        );
    }
};
