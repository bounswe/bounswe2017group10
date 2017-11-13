// @flow

import type { Action } from '../actions/index';

type State = {
  fetching: boolean,
  addCHErrors: any,
  helpOpen: boolean,
  addCHInputs: any,
  isModalOpen: boolean
}

const initState: State = {
  fetching: false,
  addCHErrors: null,
  helpOpen: true,
  addCHInputs: {},
  ImageUrl:null,
  isModalOpen: false
};

const reducer = (state: State = initState, action: Action) => {
  switch(action.type) {
    case 'FETCH_CULTURAL_HERITAGES':
      return {
        ...state,
        fetching: true
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
          [action.data.name]: action.data.value
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
        addCHErrors: action.data
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
