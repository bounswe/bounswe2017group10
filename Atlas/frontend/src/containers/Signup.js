import { connect } from 'react-redux';
import Signup from '../components/auth/Signup';
import { updateSignupInput, fetchingSignup, fetchingLogin, saveToken, loginFailed, updateUser, clearLoginInputs,  signupFailed, signupSuccess, clearSignupInputs } from '../actions/auth.js';
import axios from 'axios';
import { API_URL } from '../constants';

const mapStateToProps = state => {
    return {
        signupInputs: state.auth.signupInputs,
        signupErrors: state.auth.signupErrors,
        token: state.auth.token,

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

                    dispatch(fetchingLogin());
                    axios
                        .post(API_URL + '/api/auth/login/', {
                            username_or_email: signupInputs.username,
                            password: signupInputs.password
                        }).then(resp => {
                        const token = resp.data.token;
                        dispatch(saveToken(token))
                        axios({
                            method: 'get',
                            url: API_URL + '/api/auth/me',
                            headers: { 'Authorization': 'JWT ' + token }
                        }).then(resp => {
                            dispatch(updateUser(resp.data))
                            dispatch(clearLoginInputs());
                            // TODO: This is a bad practice for react-router. Find a better solution.
                            window.location = '/profile';

                        }).catch(err => {
                            console.log('There is an error with /api/auth/me endpoint: ' + err.data);
                        });
                    }).catch(err => {
                        const errResp = err.response.data;
                        if(errResp.non_field_errors === undefined) {
                            dispatch(loginFailed("Login Failed"));
                        } else {
                            dispatch(loginFailed(errResp.non_field_errors[0]));
                        }
                    });
                    dispatch(clearSignupInputs());
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
