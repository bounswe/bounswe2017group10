const initState = {
  username: '',
  password: '',
  token: null,
  loginFetching: false,
  loginError: null,
  signupFetching: false,
  signupError: null,
  signupSuccess: false
};
const atlas = (state = initState, action) => {
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
        [action.name]: action.value // TODO(Yigit): Do name checking
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
          ...state.signupInputs,
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
        signupError: action.text,
        signupFetching: false
      }
    default:
      return state;
  }
}

export default atlas;
