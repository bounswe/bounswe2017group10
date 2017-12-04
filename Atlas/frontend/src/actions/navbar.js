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

export const updateSearchedCulturalHeritages = (data) => {
  return {
    type: 'UPDATE_SEARCHED_CULTURAL_HERITAGES',
    data
  }
}
