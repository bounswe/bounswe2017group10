import React from 'react';
import { Route } from 'react-router-dom';
import NotFound from '../NotFound/NotFound';

const isAuthenticated = () => true;

const AuthRoute = ({ component, ...props }) => {
  if(isAuthenticated()) {
    return <Route component={ component } { ...props } />
  } else {
    return <Route component={ NotFound } />
  }
}

export default AuthRoute;
