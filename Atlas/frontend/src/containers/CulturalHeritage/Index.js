import React from 'react';
import { connect } from 'react-redux';
import IndexPage from '../../components/CulturalHeritage/Index';
import { fetchCH, finishFetchingCH, updatingGetCH, updateCHInput, addCHFetch, addCHSuccess, addCHFail, toggleAddCHModal } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritages: state.culturalHeritage.data,
  };
}

const mapDispatchToProps = dispatch => {
  return {
    loadCulturalHeritages: (token) => {
      dispatch(fetchCH());
      axios({
        method: 'get',
        url: API_URL + '/cultural_heritage_item',
        headers: { 'Authorization': 'JWT ' + token }
      }).then(resp => {
        dispatch(updatingGetCH(resp.data));
        dispatch(finishFetchingCH());
      }).catch(err => {
        console.log("Error when fetching cultural heritage items");
        console.log(err);
        dispatch(finishFetchingCH());
      });
    }
  }
}

class App extends React.Component {
  componentWillMount() {
    this.props.loadCulturalHeritages(this.props.token);
  }

  render() {
    return <IndexPage culturalHeritages={ this.props.culturalHeritages } />
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(App);

export default PageContainer;

