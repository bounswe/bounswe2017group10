export const addToken = token => {
  return {
    type: 'ADD_TOKEN',
    text: token
  }
}

export const updateInput = (name, value) => {
  return {
    type: 'UPDATE_INPUT',
    name,
    value
  }
}

export const fetching = () => {
  return {
    type: 'FETCHING'
  }
}

export const loginFailed = (err) => {
  return {
    type: 'LOGIN_FAILED',
    text: err
  }
}
