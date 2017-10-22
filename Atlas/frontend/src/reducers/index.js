import auth from './auth';
import culturalHeritage from './culturalHeritage';
import navbar from './navbar';
import { combineReducers } from 'redux';

const reducers = combineReducers({ auth, culturalHeritage, navbar });
export default reducers;
