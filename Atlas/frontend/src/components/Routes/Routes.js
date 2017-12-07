import React from 'react';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Redirect
} from 'react-router-dom'
import Home from "../Home/Home";
import LoginRoute from "../../routes/Login/LoginRoute";
import Navbar from '../../containers/Navbar/Navbar';

import Profile from "../../containers/Profile/Profile";

import PrivateRoute from './PrivateRoute';
import IndexCulturalHeritagesPage from '../../containers/CulturalHeritage/Index';
import AddCulturalHeritagePage from '../../containers/CulturalHeritage/Add';
import ShowCulturalHeritagePage from '../../containers/CulturalHeritage/Show';
import Search from '../../containers/Search/Search';
import NearbyItemsPage from '../../containers/CulturalHeritage/Nearby';
import NotFound from '../../components/NotFound/NotFound';

const Routes = ({ user, loggedIn }) => ({
  render() {
    return (
      <div>
        <Router>
            <div>
              <Navbar />
              <Switch>

                <Route exact path="/" component={ Home } />
                <Route exact path="/login" component={ LoginRoute } />
                <PrivateRoute exact path="/cultural-heritages/new" user={ user } component={ <AddCulturalHeritagePage /> } />
                <PrivateRoute path="/cultural-heritages/:id" user={ user } component={ <ShowCulturalHeritagePage /> } />
                <PrivateRoute exact path="/cultural-heritages" user={ user } component={ <IndexCulturalHeritagesPage /> } />
                <PrivateRoute exact path="/profile" user={ user } component={ <Profile /> } />
                <PrivateRoute exact path="/search" user={ user } component={ <Search /> } />
                <PrivateRoute exact path="/nearby-items" user={ user } component={ <NearbyItemsPage /> } />
                <Route exact path="/404" component={ NotFound } />
                <Redirect to="/404" />
              </Switch>
            </div>
        </Router>
      </div>
    );
  }
});

export default Routes;
