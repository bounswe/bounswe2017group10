const initState = {
  username: '',
  password: '',
  token: null,
  fetching: false,
  loginError: null
};
const atlas = (state = initState, action) => {
  switch(action.type) {
    case 'ADD_TOKEN':
      return {
        ...state,
        token: action.text,
        fething: false
      };
    case 'UPDATE_INPUT':
      return {
        ...state,
        [action.name]: action.value
      }
    case 'FETCHING':
      return {
        ...state,
        fetching: true,
        loginError: null
      }
    case 'LOGIN_FAILED':
      return {
        ...state,
        fetching: false,
        loginError: action.text
      }
    default:
      return state;
  }
}

export default atlas;
