import React from 'react';
import { connect } from 'react-redux';
import ShowPage from '../../components/CulturalHeritage/Show';
import { withRouter } from 'react-router';
import { authGet, authPost } from '../../utils';
import { API_URL } from '../../constants';
import { updateCommentInput, updateCulturalHeritage } from '../../actions/culturalHeritage';
import { favItem } from './Common';

const mapStateToProps = (state, props) => {
  const culturalHeritage = state.culturalHeritage.data.find(c => c.id === parseInt(props.match.params.id, 10));
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritage,
    commentInput: state.culturalHeritage.commentInput
  };
}

const mapDispatchToProps = dispatch => ({
  loadCulturalHeritage: (token, id) => {
    authGet(token, {
      url: API_URL + '/cultural_heritage_item/' + id
    }).then(resp => {
      dispatch(updateCulturalHeritage(id, resp.data));
    }).catch(err => {
      console.log("Error when fetching cultural heritage item");
      console.log(err);
    });
  },
  commentInputChange: (event) => {
    dispatch(updateCommentInput(event.target.value));
  },
  postComment: (token, culturalHeritageId, text) => {
    authPost(token, {
      url: API_URL + '/cultural_heritage_item/' + culturalHeritageId + '/comment',
      data: {
        comment: {
          text: text
        }
      }
    }).then(response => {
      authGet(token, {
        url: API_URL + '/cultural_heritage_item/' + culturalHeritageId
      }).then(resp => {
        dispatch(updateCulturalHeritage(culturalHeritageId, resp.data));
      })
      .catch(err => {
        console.log("Error when trying to refresh cultural heritage after commenting");
      })
    }).catch(err => {
      console.log("Error when trying to comment on cultural heritage");
    });
  },
  favoriteItem: (token, culturalHeritage) => {
    favItem(dispatch, token, culturalHeritage);
  }
});

class App extends React.Component {
  componentWillMount() {
    this.props.loadCulturalHeritage(this.props.token, this.props.match.params.id);
  }

  render() {
    return <ShowPage culturalHeritage={ this.props.culturalHeritage } { ...this.props }  />
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(App);

export default withRouter(PageContainer);
