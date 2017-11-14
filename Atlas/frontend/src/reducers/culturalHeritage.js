const initState = {
  fetching: false,
  fetchingUserItems: false,
  addCHErrors: null,
  helpOpen: true,
  addCHInputs: {},
  ImageUrl: null,
  userItems: null,
};

const reducer = (state = initState, action) => {
  switch(action.type) {
    case 'FETCH_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: true
      }
    case 'FETCH_USER_ITEMS':
      return {
         ...state,
         fetchingUserItems: true
    }
    case 'IMAGE_URL_UPLOADED':
      return {
         ...state,
         ImageUrl: action.data
    }
    case 'UPDATE_CULTURAL_HERITAGES':
      return {
        ...state,
        data: action.data
    }
    case 'UPDATE_USER_ITEMS':
      return {
        ...state,
        userItems: action.data
      }
    case 'FINISH_FETCHING_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: false
    }
    case 'FINISH_FETCHING_USER_ITEMS':
      return {
        ...state,
        fetchingUserItems: false
      }
    case 'UPDATE_CULTURAL_HERITAGE_INPUT':
      return {
        ...state,
        addCHInputs: {
          ...(state.addCHInputs),
          [action.name]: action.value
        }
      }
    case 'ADD_CH_FETCH':
      return {
        ...state,
        addChFetching: true,
        addCHErrors: null
      }
    case 'ADD_CH_SUCCESS':
      return {
        ...state,
        addChFetching: false,
        addCHErrors: null
      }
    case 'ADD_CH_FAIL':
      return {
        ...state,
        addChFetching: false,
        addCHErrors: action.errors
      }
    case 'TOGGLE_ADD_CH_MODAL':
      return {
        ...state,
        isModalOpen: !(state.isModalOpen)
      }
    case 'CLEAR_ADD_CH_INPUTS':
      return {
        ...state,
        addCHInputs: {}
      }
    case 'CLOSE_HELP':
      return {
        ...state,
        helpOpen: false
      }
      case 'CLEAR_ADD_CH_ERRORS':
          return {
              ...state,
              addCHErrors:{}
          }
    default:
      return state;
  }
}
export default reducer;
