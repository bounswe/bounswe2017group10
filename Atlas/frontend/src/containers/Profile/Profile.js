import { connect } from 'react-redux';
import './style.css';
import Profile from '../../components/Profile/Profile';
import axios from 'axios';
import React from 'react';
import { API_URL } from '../../constants';
import { fetchUserItems, finishFetchingUserItems, updatingUserItems } from '../../actions/culturalHeritage';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    userItems: state.culturalHeritage.userItems,
    token: state.auth.token,
  };
}

const mapDispatchToProps = dispatch => {
  return {
      loadUserHeritages: (token) => {
          dispatch(fetchUserItems());
          axios({
              method: 'get',
              url: API_URL + '/cultural_heritage_item/myitems',
              headers: { 'Authorization': 'JWT ' + token }
          }).then(resp => {
              dispatch(updatingUserItems(resp.data));
              dispatch(finishFetchingUserItems());
          }).catch(err => {
              console.log("Error when fetching cultural heritage items");
              console.log(err);
              dispatch(finishFetchingUserItems());
          });
      }
  }
}

class App extends React.Component {
    componentWillMount() {
        this.props.loadUserHeritages(this.props.token);
    }

    render() {
        return <Profile userItems={ this.props.userItems } { ...this.props }  />
    }
}

const ProfileContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(App);

export default ProfileContainer;
