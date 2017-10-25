import { connect } from 'react-redux';
import Signup from '../components/auth/Signup';
import { updateSignupInput, fetchingSignup, signupFailed, signupSuccess, clearSignupInputs } from '../actions/auth.js';
import axios from 'axios';
import { API_URL } from '../constants';

const mapStateToProps = state => {
  return {
    signupInputs: state.auth.signupInputs,
    signupErrors: state.auth.signupErrors 
  };
}

const mapDispatchToProps = dispatch => {
  return {
    inputChanged: (event) => { dispatch(updateSignupInput(event.target.name, event.target.value)); },
    attemptSignup: (signupInputs) => {
      dispatch(fetchingSignup()); 
      axios
        .post(API_URL + '/api/auth/signup/', signupInputs)
        .then(function(resp) {
          dispatch(signupSuccess());
          dispatch(clearSignupInputs());
          window.location = '/profile';
        })
        .catch(function(err) {
          dispatch(signupFailed(err.response.data));
        });
    }
  };
}

const SignupContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Signup);

export default SignupContainer;
