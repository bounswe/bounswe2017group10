import { connect } from 'react-redux';
import Routes from '../components/Routes/Routes';

const mapStateToProps = state => {
  return {
    user: state.auth.user,
    token: state.auth.token
  };
}

const mapDispatchToProps = dispatch => ({})

const RoutesContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Routes);

export default RoutesContainer;
