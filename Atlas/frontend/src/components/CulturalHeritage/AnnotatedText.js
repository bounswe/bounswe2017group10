import React from 'react';
import Paragraph from 'react-annotated-paragraph'

const AnnotatedText = ({ text, annotationInput, updateAnnotationInput, annotations }) => {

  const showSelectedText = e => {
    var text = '';
    if (window.getSelection) {
        text = window.getSelection();
    } else if (document.getSelection) {
        text = document.getSelection();
    } else if (document.selection) {
        text = document.selection.createRange().text;
    }
    if(text.toString() !== "") updateAnnotationInput({ ...annotationInput, text: text });
  }
  const mySimpleRenderer = (text, annotation) => {
    let explanation = annotation.tooltip
    let highlighted = text.substr(annotation.offset, annotation.length);
    return {
      explanation,
      highlighted
    }
  }
  const annotatedDescription =
    <Paragraph
      paragraph={{
        text: text,
        annotations: annotations.map(a => ({ offset: a.selector.start, length: a.selector.end - a.selector.start, tooltip: a.body[0].value.text }))
      }}
      tooltipRenderer={ mySimpleRenderer }
    />
  return (
    <div onMouseUp={ showSelectedText }>
      { annotatedDescription }
    </div>
  )
}

export default AnnotatedText;
