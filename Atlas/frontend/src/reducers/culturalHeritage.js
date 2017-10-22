const initState = {
  data: [
    { title: "Title 1",
      description: "Description 1"
    },
    { title: "Title 1",
      description: "Description 1"
    },
    { title: "Title 1",
      description: "Description 1"
    },
    { title: "Title 1",
      description: "Description 1"
    }
  ],
  fetching: false
};
const reducer = (state = initState, action) => {
  switch(action.type) {
    case 'FETCH_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: true
      }
    case 'UPDATE_CULTURAL_HERITAGES':
      return {
        ...state,
        data: action.data
      }
    case 'FINISH_FETCHING_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: false
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
        addChFetching: true
      }
    case 'ADD_CH_SUCCESS':
      return {
        ...state,
        addChFetching: false
      }
    case 'ADD_CH_FAIL':
      return {
        ...state,
        addChFetching: false
      }
    case 'TOGGLE_ADD_CH_MODAL':
      return {
        ...state,
        isModalOpen: !(state.isModalOpen)
      }
    default:
      return state;
  }
}
export default reducer;
