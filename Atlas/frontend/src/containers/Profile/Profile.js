import { connect } from 'react-redux';
import './style.css';
import Profile from '../../components/Profile/Profile';

const mapStateToProps = state => {
  console.log(state);
  return {
    user: state.user
  };
}

const mapDispatchToProps = dispatch => {
  return {}
}

const ProfileContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Profile);

export default ProfileContainer;
