export const fetchCH = () => {
  return {
    type: 'FETCH_CULTURAL_HERITAGES'
  }
}

export const finishFetchingCH = () => {
  return {
    type: 'FINISH_FETCHING_CULTURAL_HERITAGES'
  }
}

export const updateCH = (culturalHeritages) => {
  return {
    type: 'UPDATE_CULTURAL_HERITAGES',
    data: culturalHeritages
  }
}

