import React from 'react';
import Paragraph from 'react-annotated-paragraph'
import CloseButton from 'react-icons/lib/ti/times';
import { Input, Button } from 'reactstrap';
import { ANNOTATION_TXT_INPUT } from '../../constants';

const AnnotatedText = ({ text, annotationInput, updateAnnotationInput, annotations, token, createAnnotation, culturalHeritage }) => {

  const showSelectedText = e => {
    var text = '';
    if (window.getSelection) {
        text = window.getSelection();
    } else if (document.getSelection) {
        text = document.getSelection();
    } else if (document.selection) {
        text = document.selection.createRange().text;
    }
    console.log(text);
    if(text.toString() !== "")
      updateAnnotationInput(
        {
          ...annotationInput,
          open: true,
          start: text.anchorOffset,
          end: text.focusOffset
        }
      );
  }
  const mySimpleRenderer = (text, annotation) => {
    let explanation = annotation.tooltip
    let highlighted = text.substr(annotation.offset, annotation.length);
    return {
      explanation,
      highlighted
    }
  }
  const annots = annotations.map(a => ({ offset: text.baseNode.length + a.target[0].selector.start, length: a.target[0].selector.end - a.target[0].selector.start, tooltip: a.body[0].value.text }))
  const annotatedDescription =
    <div className="annotated-description">
      <div className="annotation-input" style={{ marginLeft: annotationInput.x, marginTop: annotationInput.y }}>
        <CloseButton onClick={ (e) => { updateAnnotationInput({ ...annotationInput, open: false }); } }/>
        <Input
          type="text"
          value={ annotationInput.text }
          onChange={ (e) => updateAnnotationInput({...annotationInput, text: e.target.value}) }
        />
        <Button onClick={ (e) => createAnnotation(ANNOTATION_TXT_INPUT, token, culturalHeritage.id, annotationInput) }>Create</Button>
      </div>
      <Paragraph
        paragraph={{
          text: text,
          annotations: annots
        }}
        tooltipRenderer={ mySimpleRenderer }
      />
    </div>
  return (
    <div onMouseUp={ showSelectedText }>
      { annotatedDescription }
    </div>
  )
}

export default AnnotatedText;
