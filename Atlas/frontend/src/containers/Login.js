import { connect } from 'react-redux';
import Login from '../components/auth/Login';
import { saveToken, updateLoginInput, fetchingLogin, loginFailed, updateUser } from '../actions/index.js';
import axios from 'axios';
import { API_URL } from '../constants';

const mapStateToProps = state => {
  return {
    token: state.token,
    loginInputs: state.loginInputs,
    loginError: state.loginError
  };
}

const mapDispatchToProps = dispatch => {
  return {
    attemptLogin: (loginInputs) => {
      dispatch(fetchingLogin());
      axios
        .post(API_URL + '/api/auth/login/', {
          username_or_email: loginInputs.username_or_email,
          password: loginInputs.password
        }).then(resp => {
          const token = resp.data.token;
          dispatch(saveToken(token))
          axios({
            method: 'get',
            url: API_URL + '/api/auth/me',
            headers: { 'Authorization': 'JWT ' + token }
          }).then(resp => {
            dispatch(updateUser(resp.data))
            // TODO: This is a bad practice for react-router. Find a better solution.
            window.location = '/profile';
          }).catch(err => {
            console.log(err);
            console.log('There is an error with /api/auth/me endpoint: ' + err.data);
          });
        }).catch(err => {
          const errResp = err.response.data;
          console.log(errResp.non_field_errors === undefined);
          if(errResp.non_field_errors === undefined) {
            dispatch(loginFailed("Login Failed"));
          } else {
            dispatch(loginFailed(errResp.non_field_errors[0]));
          }
        });
    },
    handleInputChange: (event) => {
      const target = event.target;
      const name = target.name;
      const value = target.value;
      dispatch(updateLoginInput(name, value));
    }
  }
}

const LoginContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Login);

export default LoginContainer;
