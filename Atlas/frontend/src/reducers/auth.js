// @flow

import type { Action } from '../actions/index';

type State = {
  loginInputs: any,
  token: ?string,
  loginFetching: boolean,
  loginErrors?: any,
  signupFetching: boolean,
  signupErrors?: any,
  signupSuccess: boolean,
  signupInputs: any,
  user: any
}

const initState: State = {
  loginInputs: {},
  token: null,
  loginFetching: false,
  loginErrors: null,
  signupFetching: false,
  signupErrors: null,
  signupSuccess: false,
  signupInputs: {},
  user: {}
};

const reducer: (State, Action) => State = (state = initState, action) => {
  switch(action.type) {
    case 'SAVE_TOKEN':
      return {
        ...state,
        token: action.data,
        fething: false
      };
    case 'UPDATE_LOGIN_INPUT':
      return {
        ...state,
        loginInputs: {
          ...(state.loginInputs),
          [action.data.name]: action.data.value // TODO(Yigit): Do name checking
        }
      }
    case 'LOGIN_FETCHING':
      return {
        ...state,
        loginFetching: true,
        loginError: null
      }
    case 'LOGIN_FAILED':
      return {
        ...state,
        loginFetching: false,
        loginErrors: action.data
      }
    case 'UPDATE_SIGNUP_INPUT':
      return {
        ...state,
        signupInputs: {
          ...(state.signupInputs),
          [action.data.name]: action.data.value // TODO(Yigit): Do name checking
        }
      }
    case 'SIGNUP_FETCHING':
      return {
        ...state,
        signupFetching: true
      }
    case 'SIGNUP_SUCCESS':
      return {
        ...state,
        signupErrors: null,
        signupSuccess: true,
        signupFetching: false
      }
    case 'SIGNUP_FAILED':
      return {
        ...state,
        signupErrors: action.data,
        signupFetching: false
      }
    case 'USER_UPDATED':
      return {
        ...state,
        user: action.data,
        loginFetching: false
      }
    case 'LOGIN_COMPLETED':
      return {
        ...state,
        loginFetching: false
      }
    case 'LOGOUT':
      return {
        ...state,
        token: null,
        user: null
      }
    case 'CLEAR_LOGIN_INPUTS':
      return {
        ...state,
        loginInputs: {}
      }
    case 'CLEAR_SIGNUP_INPUTS':
      return {
        ...state,
        signupInputs: {}
      }
    default:
      return state;
  }
}

export default reducer;
