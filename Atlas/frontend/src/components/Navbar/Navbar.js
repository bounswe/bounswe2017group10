import React from 'react';
import { Navbar, NavbarBrand, Collapse, NavItem, NavbarToggler, Nav } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import './style.css';
import AccIcon from 'react-icons/lib/md/account-circle';
import PhotoIcon from 'react-icons/lib/md/photo-album';

const AtlasNavbar = ({ logo, logout, loggedIn }) => (
  <div>
    <Navbar color="transparent" expand="md">
      <NavbarBrand href="/">
        <img src={ logo } alt="logo" className="logo" />
        Atlas
      </NavbarBrand>
      <NavbarToggler onClick={this.toggle} />
      <Collapse isOpen={true} navbar>
        <Nav className="ml-auto" navbar>
          <NavItem>
            <a href="https://github.com/bounswe/bounswe2017group10/">Github</a>
          </NavItem>
          { loggedIn ? (
              <NavItem>
                <a href="#" onClick={ logout }>Logout</a>
              </NavItem>
              ) : (
              <NavItem>
                <NavLink to="/login">Login</NavLink>
              </NavItem>
              )
          }
            {loggedIn &&
            <NavItem>
              <NavLink to="/profile"><AccIcon/> Profile</NavLink>
            </NavItem>
            }
          <NavItem>
            <NavLink to="/cultural-heritages"><PhotoIcon /> Cultural Heritages</NavLink>
          </NavItem>
        </Nav>
      </Collapse>
    </Navbar>
  </div>
);

export default AtlasNavbar;
