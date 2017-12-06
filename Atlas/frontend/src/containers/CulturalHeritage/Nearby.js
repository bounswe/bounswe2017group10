import React from 'react';
import { connect } from 'react-redux';
import axios from 'axios';
import Near from '../../components/CulturalHeritage/NearbyItems';
import { fetchNearbyCH, finishFetchingNearbyCH, updatingNearbyCH, closeHelp, updateCHPaginationNext, loadMoreCH, startLoadMore, finishLoadMore, disableLoadMore, enableLoadMore } from '../../actions/culturalHeritage';
import { updateUserLocation } from "../../actions/auth";
import { API_URL, CULTURAL_HERITAGE_PAGINATION_LIMIT } from '../../constants';
import { favItem } from './Common';

const mapStateToProps = state => {
    return {
        user: state.auth.user,
        token: state.auth.token,
        location: state.auth.user.location,
        nearbyItems: state.culturalHeritage.nearbyData,
        paginationNextUrl: state.culturalHeritage.paginationNextUrl,
        loadingMore: state.culturalHeritage.loadingMore,
        canLoadMore: state.culturalHeritage.canLoadMore
    };
}

const mapDispatchToProps = dispatch => {
    return {
        loadNearbyCulturalHeritages: (token, location) => {
            dispatch(fetchNearbyCH());
            var locationdata= {
                'longitude': location.lng ,
                'latitude': location.ltd,
            }
            axios({
                method: 'get',
                url: API_URL + '/nearby_items',
                headers: { 'Authorization': 'JWT ' + token },
                params: {
                    limit: CULTURAL_HERITAGE_PAGINATION_LIMIT,
                    'longitude': location.lng,
                    'latitude': location.ltd
                }
            }).then(resp => {
                dispatch(updatingNearbyCH(resp.data.results));
                resp.data.next === null
                    ? dispatch(disableLoadMore())
                    : dispatch(updateCHPaginationNext(resp.data.next));
                dispatch(finishFetchingNearbyCH());
            }).catch(err => {
                console.log("Error when fetching cultural heritage items");
                console.log(err);
                dispatch(finishFetchingNearbyCH());
            });
        },
        closeHelp: () => {
            dispatch(closeHelp());
        },
        loadUserLocation: (data) => {
            dispatch(updateUserLocation(data));
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
        },

    }
}

class App extends React.Component {
    componentWillMount() {
        //this.props.loadCulturalHeritages(this.props.token);
        //this.props.enableLoadMore();
    }

    render() {
        return <Near culturalHeritages={ this.props.culturalHeritages } { ...this.props }  />
    }
}

const PageContainer = connect(
    mapStateToProps,
    mapDispatchToProps
)(App);

export default PageContainer;