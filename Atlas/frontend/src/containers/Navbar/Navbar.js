import { connect } from 'react-redux';
import Navbar from '../../components/Navbar/Navbar';
import logo from '../../assets/images/logo.png';
import { logout } from '../../actions/auth';

const mapStateToProps = state => {
  return {
    logo
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
