import { connect } from 'react-redux';
import Navbar from '../../components/Navbar/Navbar';
import logo from '../../assets/images/logo.png';
import { logout } from '../../actions/auth';
import { toggleClosed , toggleOpen } from '../../actions/navbar';
import { isLoggedIn } from '../../utils';

const mapStateToProps = state => {
  return {
    logo,
      user: state.auth.user,
    loggedIn: isLoggedIn(state.auth.user),
      dropDownOpen : state.navbar.dropdownOpen,
      amcam : state.navbar.drop


  };
}

const mapDispatchToProps = dispatch => {
  return {
    logout: () => {
      dispatch(logout());
      window.location = '/';
    },
    closeDrop: () => {
      dispatch(toggleClosed());
    },
      openDrop: () => {
      dispatch(toggleOpen());
      }
  }
}

const NavbarContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Navbar);

export default NavbarContainer;
