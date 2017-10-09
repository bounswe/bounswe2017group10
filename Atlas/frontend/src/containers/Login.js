import { connect } from 'react-redux';
import Login from '../components/auth/Login';
import { saveToken, updateLoginInput, fetchingLogin, loginFailed, updateUser } from '../actions/index.js';
import axios from 'axios';
import { API_URL } from '../constants';

const mapStateToProps = state => {
  return {
    token: state.token,
    username: state.username,
    password: state.password,
    loginError: state.loginError
  };
}

const mapDispatchToProps = dispatch => {
  return {
    attemptLogin: (username, password) => {
      dispatch(fetchingLogin());
      axios
        .post(API_URL + '/api/auth/login/', {
          username,
          password
        }).then(resp => {
          const token = resp.data.token;
          dispatch(saveToken(token));
          axios({
            method: 'get',
            url: API_URL + '/api/auth/me',
            headers: { 'Authorization': 'JWT ' + token }
          }).then(resp => {
            dispatch(updateUser(resp.response.data));
          }).catch(err => {
            console.log('There is an error with /api/auth/me endpoint: ' + err.response.data);
          });
        }).catch(err => {
          dispatch(loginFailed(err.response.data.non_field_errors[0]));
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
