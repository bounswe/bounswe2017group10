import { connect } from 'react-redux';
import Navbar from '../../components/Navbar/Navbar';
import logo from '../../assets/images/logo.png';
import { logout } from '../../actions/auth';
import { toggleClosed , toggleOpen, updateSearchInput, selectSearchValue, updateSearchSuggestions } from '../../actions/navbar';
import { isLoggedIn, authGet } from '../../utils';
import { API_URL } from '../../constants.js';

const mapStateToProps = state => {
  return {
    token: state.auth.token,
    logo,
    user: state.auth.user,
    searchSuggestions: state.navbar.searchSuggestions,
    loggedIn: isLoggedIn(state.auth.user),
    dropDownOpen: state.navbar.dropdownOpen,
    searchInput: state.navbar.searchInput
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
    },
    updateSearchInput: (token, val) => {
      dispatch(updateSearchInput(val));
      (val === '')
        ? dispatch(updateSearchSuggestions([]))
        :
          authGet(token, {
            url: API_URL + '/cultural_heritage_item/search_autocorrect/' + val
          }).then(resp =>
            dispatch(updateSearchSuggestions(resp.data.results))
          ).catch(err =>
            console.log("Error on autocomplete: " + err)
          );
    },
    selectSearchValue: (chId) => {
      window.location = "/cultural-heritages/" + chId;
      // dispatch(selectSearchValue(val));
    }
  }
}

const NavbarContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Navbar);

export default NavbarContainer;
