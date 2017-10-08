import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import registerServiceWorker from './registerServiceWorker';
import 'bootstrap/dist/css/bootstrap.css';
import MyRouter from "./MyRouter";

ReactDOM.render(<MyRouter />, document.getElementById('root'));
registerServiceWorker();
