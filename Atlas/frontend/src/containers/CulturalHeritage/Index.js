import React from 'react';
import { connect } from 'react-redux';
import IndexPage from '../../components/CulturalHeritage/Index';
import { fetchCH, finishFetchingCH, updatingGetCH, closeHelp, updateCHPaginationNext, loadMoreCH, startLoadMore, finishLoadMore, disableLoadMore, enableLoadMore } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL, CULTURAL_HERITAGE_PAGINATION_LIMIT } from '../../constants';
import { favItem } from './Common';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritages: state.culturalHeritage.data,
    helpOpen: state.culturalHeritage.helpOpen,
    paginationNextUrl: state.culturalHeritage.paginationNextUrl,
    loadingMore: state.culturalHeritage.loadingMore,
    canLoadMore: state.culturalHeritage.canLoadMore
  };
}

const mapDispatchToProps = dispatch => {
  return {
    loadCulturalHeritages: (token) => {
      dispatch(fetchCH());
      axios({
        method: 'get',
        url: API_URL + '/cultural_heritage_item',
        headers: { 'Authorization': 'JWT ' + token },
        params: { limit: CULTURAL_HERITAGE_PAGINATION_LIMIT }
      }).then(resp => {
        dispatch(updatingGetCH(resp.data.results));
        resp.data.next === null
          ? dispatch(disableLoadMore())
          : dispatch(updateCHPaginationNext(resp.data.next));
        dispatch(finishFetchingCH());
      }).catch(err => {
        console.log("Error when fetching cultural heritage items");
        console.log(err);
        dispatch(finishFetchingCH());
      });
    },
    closeHelp: () => {
      dispatch(closeHelp());
    },
    loadMore: (token, paginationNextUrl) => {
      dispatch(startLoadMore());
      axios({
        method: 'get',
        url: paginationNextUrl,
        headers: { 'Authorization': 'JWT ' + token }
      }).then(resp => {
        dispatch(loadMoreCH(resp.data.results));
        resp.data.next === null
          ? dispatch(disableLoadMore())
          : dispatch(updateCHPaginationNext(resp.data.next));
        dispatch(finishLoadMore());
      }).catch(err => {
        console.log("Error when loading more: " + err);
        dispatch(finishLoadMore());
      });
    },
    enableLoadMore: () => {
      dispatch(enableLoadMore());
    },
    favoriteItem: (token, culturalHeritage) => {
      favItem(dispatch, token, culturalHeritage);
    }
  }
}

class App extends React.Component {
  componentWillMount() {
    this.props.loadCulturalHeritages(this.props.token);
    this.props.enableLoadMore();
  }

  render() {
    return <IndexPage culturalHeritages={ this.props.culturalHeritages } { ...this.props }  />
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(App);

export default PageContainer;
