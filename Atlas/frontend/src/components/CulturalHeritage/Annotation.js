import React from 'react';
import Marker from 'react-icons/lib/ti/bookmark';

const Annotation = ({ annotation, showAnnotation, hideAnnotation }) => (
  <div onMouseEnter={ () => showAnnotation() } onMouseLeave={ () => hideAnnotation() } className="annotation" style={{ left: annotation.target[0].selector.x, top: annotation.target[0].selector.y }}>
    <Marker />
    { annotation.display &&
        <div className="annotation-title">{ annotation.body[0].value.text }</div>
    }
  </div>
)

export default Annotation;
