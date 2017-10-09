export const saveToken = token => {
  return {
    type: 'SAVE_TOKEN',
    text: token
  }
}

export const updateLoginInput = (name, value) => {
  return {
    type: 'UPDATE_LOGIN_INPUT',
    name,
    value
  }
}

export const fetchingLogin = () => {
  return {
    type: 'LOGIN_FETCHING'
  }
}

export const loginFailed = (err) => {
  return {
    type: 'LOGIN_FAILED',
    text: err
  }
}

export const updateSignupInput = (name, value) => {
  return {
    type: 'UPDATE_SIGNUP_INPUT',
    name,
    value
  }
}

export const fetchingSignup = () => {
  return {
    type: 'SIGNUP_FETCHING'
  }
}

export const signupSuccess = () => {
  return {
    type: 'SIGNUP_SUCCESS'
  }
}

export const signupFailed = (err) => {
  return {
    type: 'SIGNUP_FAILED',
    text: err
  }
}
