import React from 'react';

const CulturalHeritage = ({ culturalHeritage }) => (
  <div className="cultural-heritage">
    <h2>{ culturalHeritage.title }</h2>
    <hr />
    <p>{ culturalHeritage.description }</p> 
  </div>
)

export default CulturalHeritage;
