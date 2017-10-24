import { TRUNCATE_LENGTH } from './constants';

export const isLoggedIn = (user) =>
  user !== null && user !== undefined

export const truncate = (str) => str.substring(0, TRUNCATE_LENGTH) + "..."
