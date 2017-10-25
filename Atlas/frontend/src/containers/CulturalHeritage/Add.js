import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Add';
import { updateCHInput, addCHFetch, addCHSuccess, addCHFail, clearAddCHInputs } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token,
    addCHInputs: state.culturalHeritage.addCHInputs,
    addCHErrors: state.culturalHeritage.addCHErrors
  };
}

const addSuccess = (dispatch) => {
  dispatch(addCHSuccess());
  dispatch(clearAddCHInputs());
  window.location = '/cultural-heritages';
}

const mapDispatchToProps = dispatch => {
  return {
    handleCHInputChange: (event) => {
      const target = event.target;
      const name = target.name;
      const value = target.value;
      dispatch(updateCHInput(name, value));
    },
    addCH: (addCHInputs, token) => {
      dispatch(addCHFetch());
      axios({
        method: 'post',
        url: API_URL + '/cultural_heritage_item',
        headers: { 'Authorization': 'JWT ' + token },
        data: {
          title: addCHInputs.title,
          description: addCHInputs.description
        }}).then(resp => {
          if(addCHInputs.img_url !== undefined && addCHInputs.img_url !== "") {
            axios({
              method: 'post',
              url: API_URL + "/cultural_heritage_item/" + resp.data.id + "/image",
              headers: { 'Authorization': 'JWT ' + token },
              data: {
                images: [{ url: addCHInputs.img_url }]
              }}
            ).then(_resp => {
              addSuccess(dispatch);
            })
          } else {
            addSuccess(dispatch);
          }
        }).catch(err => {
          dispatch(addCHFail(err.response.data));
        });
    }
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;
