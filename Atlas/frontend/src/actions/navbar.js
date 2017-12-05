export const toggleOpen = () => {
  return {
    type: 'OPEN_DROPDOWN'
  }
}

export const toggleClosed = () => {
  return {
    type: 'CLOSE_DROPDOWN'
  }
}

export const updateSearchInput = (data) => {
  return {
    type: 'UPDATE_SEARCH_INPUT',
    data
  }
}

export const selectSearchValue = (data) => {
  return {
    type: 'SELECT_SEARCH_VALUE',
    data
  }
}

export const updateSearchSuggestions = (data) => {
  return {
    type: 'UPDATE_SEARCH_SUGGESTIONS',
    data
  }
}
