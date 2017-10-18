import React from 'react';

const Page = ({ user }) => ({
  render() {
    return (
      <div>CH: { user.username }</div>
      )
  }
})

export default Page;

