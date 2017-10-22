import React from 'react';
import { connect } from 'react-redux';
import ShowPage from '../../components/CulturalHeritage/Show';
import { fetchCH, finishFetchingCH, updatingGetCH } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';
import { withRouter } from 'react-router';

const mapStateToProps = (state, props) => {
  const culturalHeritage = state.culturalHeritage.data.find(c => c.id === parseInt(props.match.params.id));
  console.log(typeof state.culturalHeritage.data[0].id);
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritage,
  };
}

const mapDispatchToProps = dispatch => ( {} )

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(ShowPage);

export default withRouter(PageContainer);
