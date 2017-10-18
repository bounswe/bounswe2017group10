import React from 'react';

const Page = ({ user, token, culturalHeritages, loadCulturalHeritages }) => ({
  componentDidMount() {
    loadCulturalHeritages(token);
  },

  render() {
    return (
      <div>
        { culturalHeritages.map(c => (<span key={ c.toString }>{ c }</span>))} 
      </div>
      )
  }
})

export default Page;

