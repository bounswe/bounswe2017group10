import React from 'react';
import {
    BrowserRouter as Router,
    Route
} from 'react-router-dom'
import Home from "../Home/Home";
import LoginRoute from "../../routes/Login/LoginRoute";
import Navbar from '../Navbar/Navbar';
import atlas from '../../assets/images/atlas.jpeg';
import logo from '../../assets/images/logo.png';
import Profile from "../../routes/Profile/Profile";
import AuthRoute from './AuthRoute';

const Routes = ({ token, fetchUser }) => (
  <div>
    <img src={atlas} className="background-image" alt="background" />
    <Navbar logo={ logo } token={ token }/>
    <Router>
        <div>
            <Route exact path="/" component={ Home } />
            <Route path="/login" component={ LoginRoute } />
            <AuthRoute path="/profile" component={ Profile } />
        </div>
    </Router>
  </div>
)

export default Routes;
