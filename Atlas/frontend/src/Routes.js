import React, { Component } from 'react';
import {
    BrowserRouter as Router,
    Route
} from 'react-router-dom'
import Home from "./components/Home/Home";
import LoginRoute from "./routes/Login/LoginRoute";
import Navbar from './components/Navbar/Navbar';
import atlas from './assets/images/atlas.jpeg';
import logo from './assets/images/logo.png';

export default class Routes extends Component {
    constructor(props) {
      super(props);
      this.state = { token: null }
    }
    
    componentDidMount() {
      this.state = localStorage.getItem('data');
    }

    saveToken(token) {
      console.log("Saving token: " + token);
      this.setState({ token: token });
      localStorage.setItem('data', { token: token });
    }

    render(){
        return (
            <div>
              <img src={atlas} className="background-image" alt="background" />
              <Navbar logo={ logo } token={ this.state.token }/>
              <Router>
                  <div>
                      <Route
                        exact
                        path="/"
                        render={ (props) => (
                          <Home token={ this.state.token }  />
                        )}
                      />
                      <Route
                        path="/login"
                        render={(props) => (
                          <LoginRoute saveToken={ this.saveToken.bind(this) } />
                        )}
                      />
                  </div>
              </Router>
            </div>
        );
    }
};
