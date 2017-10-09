import { connect } from 'react-redux';
import Login from '../components/auth/Login';
import { saveToken, updateLoginInput, fetchingLogin, loginFailed } from '../actions/index.js';
import axios from 'axios';

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
        .post('http://localhost:8000/api/auth/login', {
          username,
          password
        })
        .then(function(resp) {
          saveToken(resp.data.token)
        })
        .catch(function(err) {
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
