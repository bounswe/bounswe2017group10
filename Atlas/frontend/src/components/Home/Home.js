import React, { Component } from 'react';
import './style.css';
import { NavLink } from 'react-router-dom';
import atlasImg from '../../assets/images/atlas.jpeg';
import DownArrow from 'react-icons/lib/fa/angle-double-down';

class Home extends Component {
  render() {
    return (
      <div className="Home">
        <img src={atlasImg} className="background-image" alt="background" />
        <header className="Home-header">
          <h1 className="Home-title">Enhance </h1>
          <h1 className="Home-title">The </h1>
          <h1 className="Home-title">Culture</h1>
          <h2 className="Home-title">Together.</h2>
        </header>
        <footer className="Home-footer">
          <NavLink className="down-link" to="/cultural-heritages">
            See More
            <br />
            <DownArrow />
          </NavLink>
        </footer>
      </div>
    );
  }
}

export default Home;
