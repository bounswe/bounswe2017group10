import Search from '../../components/Search/Search';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    culturalHeritages: state.navbar.searchedCulturalHeritages
  };
}

const mapDispatchToProps = dispatch => {
  return {
  }
}

const SearchContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Search);

export default SearchContainer;
