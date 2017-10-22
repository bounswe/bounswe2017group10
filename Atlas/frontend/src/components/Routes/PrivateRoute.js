import React from 'react';
import { Route, Redirect } from 'react-router-dom';
import { isLoggedIn } from '../../utils';

const PrivateRoute = ({ component, user, ...rest }) => (
  <Route
    {...rest}
    render={ (props) => isLoggedIn(user)
      ? component
      : <Redirect to={{pathname: '/login', state: {from: props.location}}} />
    }
  />
)
export default PrivateRoute;
