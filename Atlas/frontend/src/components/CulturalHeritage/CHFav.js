import React from 'react';
import FilledHeart from 'react-icons/lib/fa/heart';
import EmptyHeart from 'react-icons/lib/fa/heart-o';

const CHFav = ({ culturalHeritage, favorite }) => (
  <div className="fav-indicator" style={{ zIndex: 10, display: 'inline-block' }}>
    <span>{ culturalHeritage.favorited_amount }</span>
    { culturalHeritage.is_favorite
    ?  <FilledHeart onClick={ favorite }/>
    : <EmptyHeart onClick={ favorite }/>
    }
  </div>
)

export default CHFav;
