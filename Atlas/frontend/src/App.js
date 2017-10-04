import React, { Component } from 'react';
import atlas from './assets/images/atlas.jpeg';
import logo from './assets/images/logo.png';
import { AtlasNavbar } from './Components';
import './App.css';

class App extends Component {
  render() {
    return (
      <div className="App">
        <img src={atlas} className="background-image" alt="background" />
        <AtlasNavbar logo={logo} />
      </div>
    );
  }
}

export default App;
