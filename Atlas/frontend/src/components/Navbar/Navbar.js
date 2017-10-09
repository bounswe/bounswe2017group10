import React, { Component } from 'react';
import { Navbar, NavbarBrand, Collapse, NavItem, NavLink, NavbarToggler, Nav } from 'reactstrap';

export default class AtlasNavbar extends Component {

  toggle() {
    this.setState({
      isOpen: !this.state.isOpen
    });
  }
  render() {
    return (
      <div>
        <Navbar color="transparent" expand="md">
            <NavbarBrand href="/">
              <img src={ this.props.logo } alt="logo" className="logo" />
              Atlas
            </NavbarBrand>
          <NavbarToggler onClick={this.toggle} />
          <Collapse isOpen={true} navbar>
            <Nav className="ml-auto" navbar>
              <NavItem>
                <NavLink href="https://github.com/bounswe/bounswe2017group10/">Github</NavLink>
              </NavItem>
              <NavItem>
                <NavLink href="/login">Login</NavLink>
              </NavItem>
            </Nav>
          </Collapse>
        </Navbar>
      </div>
    );
  }
};

