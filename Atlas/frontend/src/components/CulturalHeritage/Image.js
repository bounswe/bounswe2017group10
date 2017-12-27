import React from 'react';
import Annotation from './Annotation';
import { Input, Button } from 'reactstrap';
import CloseButton from 'react-icons/lib/ti/times';
import { ANNOTATION_IMG_INPUT } from '../../constants';

const Image = ({ src, annotations, showAnnotation, hideAnnotation, annotationInput, updateAnnotationInput, createAnnotation, showAnnotations, openAnnotationInput, closeAnnotationInput, token, culturalHeritage }) => (
  <div
    className="annotations-container"
    onClick={
      (e) =>
      showAnnotations &&
      !annotationInput.open &&
      updateAnnotationInput({ ...annotationInput, open: true, x: e.nativeEvent.offsetX, y: e.nativeEvent.offsetY })
    }

  >
    { showAnnotations &&
      <div>
        { annotationInput.open &&
          <div className="annotation-input" style={{ marginLeft: annotationInput.x, marginTop: annotationInput.y }}>
            <CloseButton onClick={ (e) => { updateAnnotationInput({ ...annotationInput, open: false }); } }/>
            <Input
              type="text"
              value={ annotationInput.text }
              onChange={ (e) => updateAnnotationInput({...annotationInput, text: e.target.value}) }
            />
            <Button onClick={ (e) => createAnnotation(ANNOTATION_IMG_INPUT, token, culturalHeritage.id, annotationInput) }>Create</Button>
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
