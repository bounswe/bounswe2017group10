import { connect } from 'react-redux';
import Navbar from '../../components/Navbar/Navbar';
import logo from '../../assets/images/logo.png';
import { logout } from '../../actions/auth';
import { isLoggedIn } from '../../utils';

const mapStateToProps = state => {
  return {
    logo,
    loggedIn: isLoggedIn(state.auth.user)
  };
}

const mapDispatchToProps = dispatch => {
  return {
    logout: () => {
      dispatch(logout());
      window.location = '/';
    }
  }
}

const NavbarContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Navbar);

export default NavbarContainer;
