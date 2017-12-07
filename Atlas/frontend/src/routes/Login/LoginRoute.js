import React, { Component } from 'react';
import {Col, Row, Container} from 'reactstrap';
import Signup from "../../containers/Signup";
import Login from "../../containers/Login";
import './style.css';
import atlasImg from '../../assets/images/atlas.jpeg';
export default class LoginRoute extends Component {
    render() {

        return (
        <Container>

            <img src={atlasImg} className="background-image" alt="background" />
            <Row className="login-container">
                <Col xs="5" className="login-column">
                  <Login />
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
