const initState = {
  loginInputs: {},
  token: null,
  loginFetching: false,
  loginError: null,
  signupFetching: false,
  signupErrors: null,
  signupSuccess: false,
  signupInputs: {},
  user: {}
};
const reducer = (state = initState, action) => {
  switch(action.type) {
    case 'SAVE_TOKEN':
      return {
        ...state,
        token: action.text,
        fething: false
      };
    case 'UPDATE_LOGIN_INPUT':
      return {
        ...state,
        loginInputs: {
          ...(state.loginInputs),
          [action.name]: action.value // TODO(Yigit): Do name checking
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
        loginError: action.text
      }
    case 'UPDATE_SIGNUP_INPUT':
      return {
        ...state,
        signupInputs: {
          ...(state.signupInputs),
          [action.name]: action.value // TODO(Yigit): Do name checking
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
        signupSuccess: true,
        signupFetching: false
      }
    case 'SIGNUP_FAILED':
      return {
        ...state,
        signupErrors: action.errors,
        signupFetching: false
      }
    case 'USER_UPDATED':
      console.log(action.data);
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
    default:
      return state;
  }
}

export default reducer;
