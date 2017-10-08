import React, { Component } from 'react';
import {
    BrowserRouter as Router,
    Route,
    Link
} from 'react-router-dom'
import {Home} from "./Components";
import AtlasLogin from "./components/AtlasLogin";


export default class MyRouter extends Component {

    render(){
        return (
            <Router>
                <div>
                    <Route exact path="/" component={Home}/>
                    <Route path="/login" component={AtlasLogin}/>
                </div>
            </Router>
        );
    }
};