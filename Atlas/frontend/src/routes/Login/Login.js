import React, { Component } from 'react';
import {Col, Row, Container} from 'reactstrap';
import atlas from '../../assets/images/atlas.jpeg';
import Signup from "../../components/auth/Signup";
import Login from "../../components/auth/Login";
import logo from '../../assets/images/logo.png';
import Search from "../../components/Search/Search";
import './style.css';

export default class LoginRoute extends Component {

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
            <img src={atlas} className="background-image" alt="background" />
            <Search logo={logo} />
            <Row className="login-container">
                <Col xs="5" className="login-column">
                  <Login/>
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
