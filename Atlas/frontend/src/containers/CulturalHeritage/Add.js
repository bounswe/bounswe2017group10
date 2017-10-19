import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Add';
import { fetchCH, finishFetchingCH, updatingGetCH, updateCHInput, addCHFetch, addCHSuccess, addCHFail, toggleAddCHModal } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';

const mapStateToProps = state => {
  console.log(state.culturalHeritage.addCHInputs);
  return {
    user: state.auth.user,
    token: state.auth.token,
    addCHInputs: state.culturalHeritage.addCHInputs,
  };
}

const mapDispatchToProps = dispatch => {
  return {
    handleCHInputChange: (event) => {
      const target = event.target;
      const name = target.name;
      const value = target.value;
      dispatch(updateCHInput(name, value));
    },
    createCH: (addCHInputs, token) => {
      console.log(addCHInputs);
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
          window.location = '/cultural-heritages';
        }).catch(err => {
          dispatch(addCHFail());
        });
    }
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;


