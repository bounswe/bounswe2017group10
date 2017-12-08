import { TRUNCATE_LENGTH } from './constants';
import React, { Component } from 'react';
import { Alert } from 'reactstrap';
import axios from 'axios';

export const isLoggedIn = (user) =>
  user !== null && user !== undefined

export const truncate = (str, len) => str.substring(0, len ? len : TRUNCATE_LENGTH) + " ..."

export class Errors extends Component {
  render() {
    const errs = this.props.errors;
    return (
      <div className="atlas-errors">
        { errs && typeof errs === 'object'
        ? Object.entries(errs).map(entr =>
            ( <Alert key={ entr[0] } color="danger">{ entr[0] }: { entr[1][0] }</Alert> )
          )
        : '' 
        }
      </div>
    )
  }
}

export const authGet = (token, opts) =>
  axios({
    method: 'get',
    url: opts.url, 
    headers: { 'Authorization': 'JWT ' + token }
  })

export const authPost = (token, opts) =>
  axios({
    method: 'post',
    url: opts.url, 
    headers: { 'Authorization': 'JWT ' + token },
    data: opts.data
  })

export const authDelete = (token, opts) =>
  axios({
    method: 'delete',
    url: opts.url, 
    headers: { 'Authorization': 'JWT ' + token },
    data: opts.data
  })

export const getUrlParam = (name) => {
  let regex = new RegExp('[\\?&]' + name + '=([^&#]*)');
  let results = regex.exec(window.location.search);
  return results === null ? '' : decodeURIComponent(results[1].replace(/\+/g, ' '));
};

