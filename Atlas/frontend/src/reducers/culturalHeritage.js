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
    default:
      return state;
  }
}
export default reducer;
