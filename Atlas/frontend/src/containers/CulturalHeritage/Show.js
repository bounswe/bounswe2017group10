import { connect } from 'react-redux';
import ShowPage from '../../components/CulturalHeritage/Show';
import { withRouter } from 'react-router';

const mapStateToProps = (state, props) => {
  const culturalHeritage = state.culturalHeritage.data.find(c => c.id === parseInt(props.match.params.id, 10));
  return {
    user: state.auth.user,
    token: state.auth.token,
    culturalHeritage,
  };
}

const mapDispatchToProps = dispatch => ( {} )

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(ShowPage);

export default withRouter(PageContainer);
