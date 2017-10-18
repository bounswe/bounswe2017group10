import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Page';
import { fetchCH, finishFetchingCH, updateCH } from '../../actions/culturalHeritage';
import axios from 'axios';
import { API_URL } from '../../constants';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritages: state.culturalHeritage.data
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
        dispatch(updateCH(resp.data));
        dispatch(finishFetchingCH());
      }).catch(err => {
        console.log("Error when fetching cultural heritage items");
        console.log(err);
        dispatch(finishFetchingCH());
      });
    }
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;

