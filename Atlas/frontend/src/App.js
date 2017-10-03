import React, { Component } from 'react';
import logo from './logo.svg';
import { SuccessButton, AtlasNavbar } from './Components';
import './App.css';

class App extends Component {
  render() {
    return (
      <div className="App">
        <AtlasNavbar />
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to Atlas Project</h1>
        </header>
      </div>
    );
  }
}

export default App;
