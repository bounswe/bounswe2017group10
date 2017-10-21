import React, { Component } from 'react';
import './style.css';
import { NavLink } from 'react-router-dom';
import atlasImg from '../../assets/images/atlas.jpeg';

class Home extends Component {
  render() {
    return (
      <div className="Home">
        <img src={atlasImg} className="background-image" alt="background" />
        <header className="Home-header">
          <h1 className="Home-title">Enhance The Culture</h1>
          <h1 className="Home-title">Together.</h1>
        </header>
        <footer className="Home-footer">
          <NavLink to="/cultural-heritages">See More</NavLink>
        </footer>
      </div>
    );
  }
}

export default Home;
