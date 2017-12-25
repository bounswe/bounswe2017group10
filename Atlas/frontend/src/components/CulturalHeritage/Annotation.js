import React from 'react';
import Marker from 'react-icons/lib/ti/bookmark';

const Annotation = ({ annotation, showAnnotation, hideAnnotation }) => (
  <div onMouseEnter={ () => showAnnotation() } onMouseLeave={ () => hideAnnotation() } className="annotation" style={{ left: annotation.x, top: annotation.y }}>
    <Marker />
    { annotation.display &&
        <div className="annotation-title">{ annotation.title }</div>
    }
  </div>
)

export default Annotation;
