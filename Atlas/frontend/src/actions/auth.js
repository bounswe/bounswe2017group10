export const saveToken = token => {
  return {
    type: 'SAVE_TOKEN',
    data: token
  }
}

export const updateLoginInput = (name, value) => {
  return {
    type: 'UPDATE_LOGIN_INPUT',
    data: {
      name,
      value
    }
  }
}

export const fetchingLogin = () => {
  return {
    type: 'LOGIN_FETCHING'
  }
}

export const loginFailed = (errors) => {
  return {
    type: 'LOGIN_FAILED',
    data: errors
  }
}

export const updateSignupInput = (name, value) => {
  return {
    type: 'UPDATE_SIGNUP_INPUT',
    data: {
      name,
      value
    }
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
    data: err
  }
}

export const updateUser = (data) => {
  return {
    type: 'USER_UPDATED',
    data
  }
}

export const logout = () => {
  return {
    type: 'LOGOUT'
  }
}

export const clearLoginInputs = () => {
  return {
    type: 'CLEAR_LOGIN_INPUTS'
  }
}

export const clearSignupInputs = () => {
  return {
    type: 'CLEAR_SIGNUP_INPUTS'
  }
}
