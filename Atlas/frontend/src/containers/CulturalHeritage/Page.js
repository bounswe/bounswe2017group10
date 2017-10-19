import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Page';
import { fetchCH, finishFetchingCH, updatingGetCH, updateCHInput, addCHFetch, addCHSuccess, addCHFail, toggleAddCHModal } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritages: state.culturalHeritage.data,
    addCHInputs: state.culturalHeritage.addCHInputs,
    isModalOpen: state.culturalHeritage.isModalOpen
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
    },
    handleCHInputChange: (event) => {
      const target = event.target;
      const name = target.name;
      const value = target.value;
      dispatch(updateCHInput(name, value));
    },
    createCH: (addCHInputs, token) => {
      dispatch(addCHFetch());
      axios({
        method: 'post',
        url: API_URL + '/cultural_heritage_item',
        headers: { 'Authorization': 'JWT ' + token },
        data: {
          title: addCHInputs.title,
          description: addCHInputs.description
        }}).then(resp => {
          dispatch(addCHSuccess());
        }).catch(err => {
          dispatch(addCHFail());
        });
    },
    toggleAddCHModal: () => {
      dispatch(toggleAddCHModal());
    }
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;

