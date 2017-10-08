import React, { Component } from 'react';
import {Button, Form, FormGroup, Input, Col, Row} from 'reactstrap';


export default class LoginSearch extends Component {

    constructor(props){
        super(props);
        this.state = {searchvalue: ''};

        this.handleChange = this.handleChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }


    handleChange(event){
        this.setState({searchvalue: event.target.searchvalue});
    }

    handleSubmit(){
        //request server and redirect to searched page
    }

    render() {
        return (
            <Row>
                <Col xs="1">
                    <img src={ this.props.logo } alt="logo" className="logo" />
                    Atlas
                </Col>
                <Col xs="9">
                    <Input type="search" name="search" value={this.state.searchvalue} placeholder="Search" onChange={this.handleChange} />
                </Col>
                <Col xs="2">
                    <Button onClick="handleSubmit">Search</Button>
                </Col>
            </Row>
        );
    }

}