import React from 'react';
import { connect } from 'react-redux';
import ShowPage from '../../components/CulturalHeritage/Show';
import { withRouter } from 'react-router';
import { authGet, authPost, authDelete } from '../../utils';
import { API_URL, ANNOTATION_SERVER_URL, ANNOTATION_TXT_INPUT, ANNOTATION_IMG_INPUT } from '../../constants';
import {
  updateCommentInput,
  updateCulturalHeritage,
  updateRecommendations,
  deleteCulturalHeritage,
  loadSingleItem,
  startUpdateRecommendation,
  showAnnotation,
  hideAnnotation,
  hideAnnotations,
  updateAnnotationInput,
  createAnnotation,
  openAnnotationInput,
  closeAnnotationInput,
  updateAnnotations
} from '../../actions/culturalHeritage';
import { favItem, getRecommendedItems } from './Common';
import NotFoundPage from '../../components/NotFound/NotFound';
import Spinner from 'react-icons/lib/fa/spinner';
import '../../components/CulturalHeritage/style.css';

const mapStateToProps = (state, props) => {
  const culturalHeritage = state.culturalHeritage.data.find(c => c.id === parseInt(props.match.params.id, 10));
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritage,
    currentItem: state.culturalHeritage.currentItem,
    commentInput: state.culturalHeritage.commentInput,
    recommendations: state.culturalHeritage.recommendations,
    recommendationLoadCompleted: state.culturalHeritage.recommendationLoadCompleted,
    annotations: state.culturalHeritage.annotations,
    annotationInput: state.culturalHeritage.annotationInput
  };
}

const fetchAnnotations = (dispatch, token, culturalHeritageId, cb) => {
  authGet(token, {
    url: ANNOTATION_SERVER_URL + '/annotation/' + 'https://atlas.org/' + culturalHeritageId
  }).then(resp => {
    dispatch(updateAnnotations(resp.data));
    cb();
  }).catch(err =>
    console.log("Error when fetching annotations for cultural heritage with id " + culturalHeritageId)
  );
}

const mapDispatchToProps = dispatch => ({
  loadCulturalHeritage: (token, id, cb) => {
    authGet(token, {
      url: API_URL + '/cultural_heritage_item/' + id
    }).then(resp => {
      dispatch(updateCulturalHeritage(id, resp.data));
      //dispatch(loadSingleItem(resp.data));
      cb(resp.data);
    }).catch(err => {
      console.log("Error when fetching cultural heritage item");
    });
  },
  startLoadingRecommendations: () => {
    dispatch(startUpdateRecommendation());
  },
  updateRecommendations: (token, culturalHeritage) => {
    authGet(token, {
      url: API_URL + '/cultural_heritage_item/recommendation?item_id=' + culturalHeritage.id
    }).then(resp =>
      dispatch(updateRecommendations(resp.data.results))
    );
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
  },
    removeClick: (token, culturalHeritageId) => {

        authDelete(token, {
            url: API_URL + '/cultural_heritage_item/' + culturalHeritageId ,
        }).then(resp => {
            dispatch(deleteCulturalHeritage(culturalHeritageId));
            window.location = '/cultural-heritages';
        }).catch(err => {
                console.log("Error when trying to remove a cultural heritage");
        });

        dispatch(deleteCulturalHeritage(culturalHeritageId));
        window.location = '/cultural-heritages';

    },
  showAnnotation: (a) => {
    dispatch(hideAnnotations());
    dispatch(showAnnotation(a));
  },
  hideAnnotation: (a) => dispatch(hideAnnotation(a)),
  updateAnnotationInput: (input_type, newInput) =>
    dispatch(updateAnnotationInput(input_type, newInput)),
  fetchAnnotations_: (token, culturalHeritageId, cb) =>
    fetchAnnotations(dispatch, token, culturalHeritageId, cb),
  createAnnotation: (token, culturalHeritageId, annotationInput) => {
    dispatch(closeAnnotationInput());
    dispatch(updateAnnotationInput(ANNOTATION_IMG_INPUT, { ...annotationInput, open: false, text: "" }));
    authPost(token, {
      url: ANNOTATION_SERVER_URL + '/annotation',
      data: {
        motivation: "Referral",
        targets: [
          {
            IRI: "https://atlas.org/" + culturalHeritageId,
            type: "image",
            x: annotationInput.x,
            y: annotationInput.y
          }
        ],
        body: {
          type: "text",
          text: annotationInput.text
        }
      }
    }).then(resp => {
      fetchAnnotations(dispatch, token, culturalHeritageId, () => {})
    });
  },
  openAnnotationInput: (x, y) => dispatch(openAnnotationInput(x, y)),
  closeAnnotationInput: () => dispatch(closeAnnotationInput()),
  clearAnnotations: () => dispatch(updateAnnotations([]))
});

class App extends React.Component {
  componentDidMount() {
    this.props.clearAnnotations();
    this.props.startLoadingRecommendations();
    this.props.loadCulturalHeritage(
      this.props.token,
      this.props.match.params.id,
      (c) =>
        this.props.fetchAnnotations_(this.props.token, c.id, () =>
          this.props.updateRecommendations(this.props.token, c)
        )
    );
  }

  render() {
    return (
      this.props.culturalHeritage !== undefined
        ? ( <ShowPage culturalHeritage={this.props.culturalHeritage} {...this.props}  /> )
        : ( <div className="spinner">
          <Spinner />
          <span>Loading</span>
        </div>
        )
    )
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(App);

export default withRouter(PageContainer);
