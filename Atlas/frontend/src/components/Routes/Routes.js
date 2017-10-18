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
import Profile from "../../containers/Profile/Profile";
import PrivateRoute from './PrivateRoute';
import CulturalHeritagesPage from '../../containers/CulturalHeritage/Page';

const Routes = ({ user, token }) => ({
  render() {
    return (
      <div>
        <img src={atlas} className="background-image" alt="background" />
        <Navbar logo={ logo } token={ token }/>
        <Router>
            <div>
              <Route exact path="/" component={ Home } />
              <Route path="/login" component={ LoginRoute } />
              <PrivateRoute path="/cultural-heritages" user={ user } component={ <CulturalHeritagesPage /> } />
              <PrivateRoute path="/profile" user={ user } component={ <Profile /> } />
            </div>
        </Router>
      </div>
    );
  }
});

export default Routes;
