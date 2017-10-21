import auth from './auth';
import culturalHeritage from './culturalHeritage';
import { combineReducers } from 'redux';

const reducers = combineReducers({ auth, culturalHeritage });
export default reducers;
