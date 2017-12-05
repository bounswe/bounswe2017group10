const initAddCHInputs = {
  tags: []
}
const initState = {
  fetching: false,
  addCHErrors: null,
  helpOpen: true,
  addCHInputs: initAddCHInputs,
  data: [],
  loadingMore: false,
  canLoadMore: true,
  recommendations: []
};

const reducer = (state = initState, action) => {
  switch(action.type) {
    case 'FETCH_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: true
      }
    case 'IMAGE_URL_UPLOADED':
      return {
          ...state,
          addCHInputs: {
            ...(state.addCHInputs),
            img_url: action.data
          }
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
        addCHInputs: initAddCHInputs
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
    case 'ADD_CH_TAG':
      return {
        ...state,
        addCHInputs: {
          ...(state.addCHInputs),
          tags: state.addCHInputs.tags.concat({
            id: state.addCHInputs.tags.length + 1,
            text: action.data
          })
        }
      }
    case 'DELETE_CH_TAG':
      let tags = state.addCHInputs.tags;
      tags.splice(action.data, 1);
      return {
        ...state,
        addCHInputs: {
          ...(state.addCHInputs),
          tags
        }
      }
    case 'UPDATE_CH_PAGINATION_NEXT':
      return {
        ...state,
        paginationNextUrl: action.data
      }
    case 'LOAD_MORE_CH':
      return {
        ...state,
        data: [].concat(state.data, action.data)
      }
    case 'START_LOAD_MORE':
      return {
        ...state,
        loadingMore: true
      }
    case 'FINISH_LOAD_MORE':
      return {
        ...state,
        loadingMore: false
      }
    case 'DISABLE_LOAD_MORE':
      return {
        ...state,
        canLoadMore: false
      }
    case 'ENABLE_LOAD_MORE':
      return {
        ...state,
        canLoadMore: true
      }
    case 'UPDATE_COMMENT_INPUT':
      return {
        ...state,
        commentInput: action.data
      }
    case 'UPDATE_CULTURAL_HERITAGE':
      return {
        ...state,
        data: state.data.find(c => c.id == action.data.id)
          ? state.data.map(c => parseInt(c.id, 10) === parseInt(action.data.id, 10) ? action.data.data : c)
          : [].concat(state.data, action.data.data),
        commentInput: ""
      }
    case 'UPDATE_RECOMMENDATIONS':
      return {
        ...state,
        recommendations: action.data
      }
      case 'DELETE_CULTURAL_HERITAGE':
          //state.data =
          //alert(JSON.stringify(state.data));
          //alert(JSON.stringify(state.data.find(c => c.id == 1161)));
          //state.data.find(c => c.id == action.data);
          //let heritageItems = state.data;
          //heritageItems.splice(state.data.find(c => c.id == action.data), 1);
          return {
              ...state,
              data: state.data.filter( function(c) {
                  return !(c.id == action.data);
              })
          }
    default:
      return state;
  }

}
export default reducer;
