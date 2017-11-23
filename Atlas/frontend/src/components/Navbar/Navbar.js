import React from 'react';
import { Navbar, NavbarBrand, Collapse, NavItem, NavbarToggler, NavDropdown, DropdownToggle , DropdownMenu , DropdownItem,  Nav } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import './style.css';
import AccIcon from 'react-icons/lib/md/account-circle';
import PhotoIcon from 'react-icons/lib/md/photo-album';
import GithubIcon from 'react-icons/lib/fa/github';
import Autocomplete from 'react-autocomplete';
import { truncate } from '../../utils';

const AtlasNavbar = ({ token, logo, user, searchSuggestions, loggedIn, dropDownOpen, searchInput, logout, closeDrop, openDrop, updateSearchInput, selectSearchValue }) => (
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
            <a href="https://github.com/bounswe/bounswe2017group10/">
              <GithubIcon /> Github
            </a>
          </NavItem>
          <NavItem>
            <Autocomplete
              getItemValue={(item) => String(item.id)}
              items={searchSuggestions}
              renderItem={(item, isHighlighted) =>
                <div className="searchSuggestion" key={ Math.random() } style={{ background: isHighlighted ? 'rgba(244, 173, 66, 0.3)' : 'white' }}>
                  { truncate(item.title, 40) }
                </div>
              }
              value={searchInput}
              onChange={(e) => updateSearchInput(token, e.target.value) }
              onSelect={(val) => selectSearchValue(val) }
            />
          </NavItem>
          { !loggedIn &&
              <NavItem>
                <NavLink to="/login">Login</NavLink>
              </NavItem>
          }
          { loggedIn &&
            <NavDropdown isOpen={dropDownOpen} toggle={dropDownOpen ? closeDrop : openDrop}>
              <DropdownToggle nav caret>
                  { user.username }
              </DropdownToggle>
              <DropdownMenu>
                { user.firstname && user.lastname &&
                  <div>
                    <DropdownItem header>{ user.firstname } { user.lastname }</DropdownItem>
                    <DropdownItem divider/>
                  </div>
                }
                <DropdownItem><NavLink to="/profile"><AccIcon/> Profile</NavLink></DropdownItem>
                <DropdownItem><a href="" onClick={ logout }>Logout</a></DropdownItem>
              </DropdownMenu>
            </NavDropdown>
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
