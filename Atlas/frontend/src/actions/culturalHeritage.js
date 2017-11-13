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
    data: {
      name,
      value
    }
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
    data: errors
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

export const closeHelp = () => {
  return {
    type: 'CLOSE_HELP'
  }
}
