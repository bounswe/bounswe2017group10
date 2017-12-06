export const fetchCH = () => {
  return {
    type: 'FETCH_CULTURAL_HERITAGES'
  }
}

export const clearAddChErrors = () => {
  return{
    type: 'CLEAR_ADD_CH_ERRORS'
  }
}

export const updateLocation =(loc) =>{
  return{
    type: 'UPDATE_USER_LOCATION',
    data: loc
  }
}

export const finishFetchingCH = () => {
  return {
    type: 'FINISH_FETCHING_CULTURAL_HERITAGES'
  }
}

export const uploadImage = (imageLink) => {
    return {
        type: 'IMAGE_URL_UPLOADED',
        data: imageLink
    }
}

export const updatingGetCH = (culturalHeritages) => {
  return {
    type: 'UPDATE_CULTURAL_HERITAGES',
    data: culturalHeritages
  }
}

export const updateCHInput = (name, value) => {
  return {
    type: 'UPDATE_CULTURAL_HERITAGE_INPUT',
    name,
    value
  }
}

export const addCHFetch = () => {
  return {
    type: 'ADD_CH_FETCH'
  }
}

export const addCHSuccess = () => {
  return {
    type: 'ADD_CH_SUCCESS'
  }
}

export const addCHFail = (errors) => {
  return {
    type: 'ADD_CH_FAIL',
    errors
  }
}

export const toggleAddCHModal = () => {
  return {
    type: 'TOGGLE_ADD_CH_MODAL'
  }
}

export const clearAddCHInputs = () => {
  return {
    type: 'CLEAR_ADD_CH_INPUTS'
  }
}

export const addCHTag = (name) => {
  return {
    type: 'ADD_CH_TAG',
    data: name
  }
}

export const deleteCHTag = (id) => {
  return {
    type: 'DELETE_CH_TAG',
    data: id
  }
}

export const closeHelp = () => {
  return {
    type: 'CLOSE_HELP'
  }
}

export const updateCHPaginationNext = (nextUrl) => {
  return {
    type: 'UPDATE_CH_PAGINATION_NEXT',
    data: nextUrl
  }
}

export const loadMoreCH = (data) => {
  return {
    type: 'LOAD_MORE_CH',
    data
  }
}

export const updateCommentInput = (data) => {
  return {
    type: 'UPDATE_COMMENT_INPUT',
    data
  }
}

export const startLoadMore = () => {
  return {
    type: 'START_LOAD_MORE'
  }
}

export const finishLoadMore = () => {
  return {
    type: 'FINISH_LOAD_MORE'
  }
}

export const disableLoadMore = () => {
  return {
    type: 'DISABLE_LOAD_MORE'
  }
}

export const enableLoadMore = () => {
  return {
    type: 'ENABLE_LOAD_MORE'
  }
}

export const updateCulturalHeritage = (id, data) => {
  return {
    type: 'UPDATE_CULTURAL_HERITAGE',
    data: {
      id,
      data
    }
  }
}

export const updateRecommendations = (data) => {
  return {
    type: 'UPDATE_RECOMMENDATIONS',
    data
  }
}
