import React from 'react';
import Annotation from './Annotation';
import { Input, Button } from 'reactstrap';

const Image = ({ src, annotations, showAnnotation, hideAnnotation, annotationInput, updateAnnotationInput, createAnnotation, showAnnotations }) => (
  <div onClick={ (e) => { console.log(e.nativeEvent.offsetX);} }>
    { showAnnotations &&
      <div>
        { annotationInput.open &&
          <div className="annotation-input">
            <Input type="text" value={ annotationInput.text } onChange={ updateAnnotationInput }/>
            <Button onClick={ () => createAnnotation(annotationInput.text) }>Create</Button>
          </div>
        }
        <div className="annotations">
          { annotations.map(a => (
          <Annotation key={ Math.random() } annotation={ a } showAnnotation={ () => showAnnotation(a) } hideAnnotation={ () => hideAnnotation(a) } annotationInput={ annotationInput } />
          )) }
        </div>
      </div>
    }
    <img className="ch-image" alt="Cultural Heritage" src={ src } />
  </div>
)
export default Image;
