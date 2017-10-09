import React, { Component } from 'react';
import {
    BrowserRouter as Router,
    Route
} from 'react-router-dom'
import Home from "./components/Home/Home";
import LoginRoute from "./routes/Login/Login";


export default class Routes extends Component {
    render(){
        return (
            <Router>
                <div>
                    <Route exact path="/" component={Home}/>
                    <Route path="/login" component={LoginRoute}/>
                </div>
            </Router>
        );
    }
};
