import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Page';

const mapStateToProps = state => {
  console.log(state);
  return {
    user: state.user
  };
}

const mapDispatchToProps = dispatch => {
  return {}
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;

