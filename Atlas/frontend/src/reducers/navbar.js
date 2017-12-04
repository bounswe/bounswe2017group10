const initState = {
  dropdownOpen: false,
  searchSuggestions: []
};

const reducer = (state = initState, action) => {
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

    case 'UPDATE_SEARCH_INPUT':
      return {
        ...state,
        searchInput: action.data
      }

    case 'SELECT_SEARCH_VALUE':
      return {
        ...state,
      }

    case 'UPDATE_SEARCH_SUGGESTIONS':
      return {
        ...state,
        searchSuggestions: action.data
      }

    default:
      return state;
  }
}

export default reducer;
