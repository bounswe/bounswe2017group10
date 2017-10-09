import React from 'react';
import ReactDOM from 'react-dom';
import registerServiceWorker from './registerServiceWorker';
import 'bootstrap/dist/css/bootstrap.css';
import Routes from "./Routes";
import './assets/css/style.css';
import atlas from './reducers/index';
import { Provider } from 'react-redux';
import { createStore } from 'redux';
import { loadState, saveState } from './localStorage';

const persistedState = loadState();
const store = createStore(atlas, {
  ...persistedState,
  loginError: null // Don't persist login failure
});

store.subscribe(() => {
  saveState(store.getState());
});

ReactDOM.render(
  <Provider store={ store }>
    <Routes />
  </Provider>,
  document.getElementById('root')
);
registerServiceWorker();
