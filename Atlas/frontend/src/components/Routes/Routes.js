import React from 'react';
import {
    BrowserRouter as Router,
  Route
} from 'react-router-dom'
import Home from "../Home/Home";
import LoginRoute from "../../routes/Login/LoginRoute";
import Navbar from '../../containers/Navbar/Navbar';
import Profile from "../../containers/Profile/Profile";
import PrivateRoute from './PrivateRoute';
import IndexCulturalHeritagesPage from '../../containers/CulturalHeritage/Index';
import AddCulturalHeritagePage from '../../containers/CulturalHeritage/Add';

const Routes = ({ user, token }) => ({
  render() {
    return (
      <div>
        <Router>
            <div>
              <Navbar />
              <Route exact path="/" component={ Home } />
              <Route exact path="/login" component={ LoginRoute } />
              <PrivateRoute exact path="/cultural-heritages/new" user={ user } component={ <AddCulturalHeritagePage /> } />
              <PrivateRoute exact path="/cultural-heritages" user={ user } component={ <IndexCulturalHeritagesPage /> } />
              <PrivateRoute exact path="/profile" user={ user } component={ <Profile /> } />
            </div>
        </Router>
      </div>
    );
  }
});

export default Routes;
