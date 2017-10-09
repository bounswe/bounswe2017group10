import { connect } from 'react-redux';
import Signup from '../components/auth/Signup';
import { updateSignupInput, fetchingSignup, signupFailed, signupSuccess } from '../actions/index.js';
import axios from 'axios';

const mapStateToProps = state => {
  return {
    signupInputs: state.signupInputs,
    signupErrors: state.signupErrors 
  };
}

const mapDispatchToProps = dispatch => {
  return {
    inputChanged: (event) => { dispatch(updateSignupInput(event.target.name, event.target.value)); },
    attemptSignup: (signupInputs) => {
      dispatch(fetchingSignup()); 
      axios
        .post('http://localhost:8000/api/auth/signup/', signupInputs)
        .then(function(resp) {
          dispatch(signupSuccess());
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
