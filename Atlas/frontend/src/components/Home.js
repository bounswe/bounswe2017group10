import React, { Component } from 'react';
import atlas from '../assets/images/atlas.jpeg';
import logo from '../assets/images/logo.png';
import { AtlasNavbar } from '../Components';
import '../css/Home.css';


class Home extends Component {
  render() {
    return (

      <div className="Home">
        <img src={atlas} className="background-image" alt="background" />
        <AtlasNavbar logo={logo} />
        <header className="Home-header">
          <h1 className="Home-title">Enhance The Culture</h1>
          <h1 className="Home-title">Together.</h1>
        </header>
        <footer className="Home-footer">
          <a href="#cultural-heritages">See More</a>
        </footer>
      </div>
    );
  }
}

export default Home;
