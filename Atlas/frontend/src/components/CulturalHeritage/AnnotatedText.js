import React from 'react';
import Paragraph from 'react-annotated-paragraph'

const AnnotatedText = ({ text, annotations }) => {
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
    <div>
      { annotatedDescription }
    </div>
  )
}

export default AnnotatedText;
