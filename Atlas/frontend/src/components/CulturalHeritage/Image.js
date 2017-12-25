import React from 'react';
import Annotation from './Annotation';

const Image = ({ src, annotations, showAnnotation, hideAnnotation }) => (
  <div>
    <div className="annotations">
      { annotations.map(a => (
      <Annotation key={ Math.random() } annotation={ a } showAnnotation={ () => showAnnotation(a) } hideAnnotation={ () => hideAnnotation(a) } />
      )) }
    </div>
    <img className="ch-image" alt="Cultural Heritage" src={ src } />
  </div>
)
export default Image;
