import React, { Component } from 'react';
import {Button, Form, FormGroup, Input, Col, Row, Container, Badge} from 'reactstrap';
import '../css/AtlasSignup.css';

export default class AtlasSignup extends Component {

    constructor(props){
        super(props);
        this.state = {email:'',Name:'',surname:'',username: '', password: '',confirmpassword:''};

        this.handleMailChange = this.handleMailChange.bind(this);
        this.handleNameChange = this.handleNameChange.bind(this);
        this.handleSurnameChange = this.handleSurnameChange.bind(this);
        this.handleUserNameChange = this.handleUserNameChange.bind(this);
        this.handlePasswordChange = this.handlePasswordChange.bind(this);
        this.handleConfirmPasswordChange = this.handleConfirmPasswordChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }


    handleSubmit(){
        //request server and redirect to main page or fail to log in
    }

    handleMailChange(event){
        this.setState({email: event.target.email});
    }

    handleNameChange(event){
        this.setState({Name: event.target.Name});
    }

    handleSurnameChange(event){
        this.setState({surname: event.target.surname});
    }

    handleUserNameChange(event){
        this.setState({username: event.target.username});
    }

    handlePasswordChange(event){
        this.setState({password: event.target.password});
    }

    handleConfirmPasswordChange(event){
        this.setState({confirmpassword: event.target.confirmpassword});
    }



    render() {
        return (
                <Form  className="signup-panel"  onSubmit={this.handleSubmit}>
                    <Row><h1></h1></Row>
                    <Row><h1></h1></Row>
                    <Row><h1></h1></Row>
                    <h3>Sign-up</h3>
                    <hr/>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    Email:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="text" value={this.state.email} onChange={this.handleMailChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    Your Name:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="text" value={this.state.Name} onChange={this.handleNameChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    Your Surname:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="text" value={this.state.surname} onChange={this.handleSurnameChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    User name:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="text" value={this.state.username} onChange={this.handleUserNameChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    Password:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="password" value={this.state.password} onChange={this.handlePasswordChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <FormGroup>
                        <Row>
                            <Col xs="2">
                                <label>
                                    Confirm password:
                                </label>
                            </Col>
                            <Col xs="10">
                                <Input type="password" value={this.state.confirmpassword} onChange={this.handleConfirmPasswordChange}/>
                            </Col>
                        </Row>
                    </FormGroup>
                    <Row ><h1></h1> </Row>
                    <Row ><h1></h1> </Row>
                    <Row>
                        <Col xs="2"></Col>
                        <Col xs="5">
                            <Button className="facebook-button"><Badge >f</Badge>Sign-up with Facebook</Button>
                        </Col>
                        <Col xs="1"></Col>
                        <Col xs="4">
                            <Button>Sign-up</Button>
                        </Col>
                    </Row>
                </Form>

        );
    }
};

