import { TRUNCATE_LENGTH } from './constants';
import React, { Component } from 'react';

export const isLoggedIn = (user) =>
  user !== null && user !== undefined

export const truncate = (str) => str.substring(0, TRUNCATE_LENGTH) + "..."
