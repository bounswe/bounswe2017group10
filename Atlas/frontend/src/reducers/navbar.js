const initState = {
  dropdownOpen: false,
  searchedCulturalHeritages: []
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

    case 'UPDATE_SEARCHED_CULTURAL_HERITAGES':
      return {
        ...state,
        searchedCulturalHeritages: action.data
      }

    default:
      return state;
  }
}

export default reducer;
