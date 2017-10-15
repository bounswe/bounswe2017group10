import React from 'react';

const Profile = ({ user }) => ({
  render() {
    return (
      <div>PROFILE: { user.username }</div>
      )
  }
})

export default Profile;
