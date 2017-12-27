const initAddCHInputs = {
  tags: []
}
const initState = {
  fetching: false,
  addCHErrors: null,
  helpOpen: true,
  addCHInputs: initAddCHInputs,
  data: [],
  fetchingNearbyItems: false,
  nearbyData: [],
  loadingMore: false,
  canLoadMore: true,
  recommendations: [],
  mouseOverOn: -1,
  mapLocation: null,
  recommendationLoadCompleted: false,
  annotations:
    [
      { id: 1, body: [{ type: "text", value: { text: "wow" }}], selector: { type: "imageSelector", x: 30, y: 20 }, display: false },
      { id: 2, body: [{ type: "text", value: { text: "wow annot" }}], selector: { type: "imageSelector", x: 100, y: 50 }, display: false },
      { id: 2, type: "text", body: [{ type: "text", value: { text: "wow annot 2" }}],  selector: { type: "textSelector", start: 10, end: 20 }, display: false }
    ],
  annotationInputOpen: true,
  annotationInputText: "wow",
  annotationInputX: 0,
  annotationInputY: 0
};

const reducer = (state = initState, action) => {
  switch(action.type) {
    case 'FETCH_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: true
      }
      case 'MAP_DRAGGED':
        return {
            ...state,
            mapLocation: action.data
        }
    case 'FETCH_NEARBY_CHS':
      return{
        ...state,
        fetchingNearbyItems: true
      }
      case 'MOUSE_OVER_ITEM':
      return {
          ...state,
          mouseOverOn: action.data
      }
      case 'MOUSE_OFF_ITEM':
        return {
            ...state,
            mouseOverOn: -1
        }
    case 'UPDATE_USER_LOCATION':
      return{
          ...state,
          addCHInputs: {
              ...(state.addCHInputs),
              lng: action.data.lng,
              lat: action.data.ltd
          }

    }

    case 'LOAD_CULTURAL_HERITAGE':
      return{
          ...state,
      }

    case 'IMAGE_URL_UPLOADED':
      return {
          ...state,
          addCHInputs: {
            ...(state.addCHInputs),
            img_url: action.data
          }
      }
    case 'UPDATE_NEARBY_CHS':
      return{
          ...state,
          nearbyData: action.data
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
      case 'FINISH_FETCHING_NEARBY_CHS':
      return {
          ...state,
          fetchingNearbyItems: false
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
        recommendations: action.data,
        recommendationLoadCompleted: true
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

    case 'START_UPDATE_RECOMMENDATION':
      return {
        ...state,
        recommendationLoadCompleted: false
      }
    case 'START_UPDATE_RECOMMENDATION':
      return {
        ...state,
        recommendationLoadCompleted: false
      }
    case 'SHOW_ANNOTATION':
      return {
        ...state,
        annotations: state.annotations.map(a => a.id === action.data ? { ...a, display: true } : a)
      }
    case 'HIDE_ANNOTATION':
      return {
        ...state,
        annotations: state.annotations.map(a => a.id === action.data ? { ...a, display: false } : a)
      }
    case 'UPDATE_ANNOTATION_INPUT':
      return {
        ...state,
        annotationInputText: action.data
      }
    case 'CREATE_ANNOTATION':
      return {
        ...state,
        annotations: state.annotations.concat({ id: state.annotations.length + 1, title: action.data.text, x: action.data.x, y: action.data.y })
      }
    case 'OPEN_ANNOTATION_INPUT':
      return {
        ...state,
        annotationInputOpen: true,
        annotationInputX: action.data.x,
        annotationInputY: action.data.y
      }
    case 'CLOSE_ANNOTATION_INPUT':
      return {
        ...state,
        annotationInputOpen: false
      }
    case 'HIDE_ANNOTATIONS':
      return {
        ...state,
        annotations: state.annotations.map(a => ({ ...a, display: false }))
      }
    default:
      return state;
  }

}
export default reducer;
