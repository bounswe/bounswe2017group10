// @flow

import type { Action } from '../actions/index';

type State = {
  dropdownOpen: boolean
}

const initState = {
  dropdownOpen : false
};

const reducer = (state: State = initState, action: Action) => {
  switch(action.type) {
    case 'OPEN_DROPDOWN':
      return {
        ...state,
        dropdownOpen : true
      };

    case 'CLOSE_DROPDOWN':
      return {
        ...state,
        dropdownOpen : false
      }

    default:
      return state;
  }
}

export default reducer;
