import React from 'react';
import Paragraph from 'react-annotated-paragraph'
import CloseButton from 'react-icons/lib/ti/times';
import { Input, Button } from 'reactstrap';
import { ANNOTATION_TXT_INPUT } from '../../constants';

const AnnotatedText = ({ text, annotationInput, updateAnnotationInput, annotations, token, createAnnotation, culturalHeritage }) => {

  function getCaretCharacterOffsetWithin(element) {
      var caretOffset = 0;
      var doc = element.ownerDocument || element.document;
      var win = doc.defaultView || doc.parentWindow;
      var sel;
      if (typeof win.getSelection != "undefined") {
          sel = win.getSelection();
          if (sel.rangeCount > 0) {
              var range = win.getSelection().getRangeAt(0);
              var preCaretRange = range.cloneRange();
              preCaretRange.selectNodeContents(element);
              preCaretRange.setEnd(range.endContainer, range.endOffset);
              caretOffset = preCaretRange.toString().length;
          }
      } else if ( (sel = doc.selection) && sel.type != "Control") {
          var textRange = sel.createRange();
          var preCaretTextRange = doc.body.createTextRange();
          preCaretTextRange.moveToElementText(element);
          preCaretTextRange.setEndPoint("EndToEnd", textRange);
          caretOffset = preCaretTextRange.text.length;
      }
      return caretOffset;
  }

  const showSelectedText = e => {
    var text = '';
    if (window.getSelection) {
        text = window.getSelection();
    } else if (document.getSelection) {
        text = document.getSelection();
    } else if (document.selection) {
        text = document.selection.createRange().text;
    }
    const caretPos = getCaretCharacterOffsetWithin(e.target)
    const length_ = text.focusOffset - text.anchorOffset
    const length = length_ > 0 ? length_ : -length_;
    const start = caretPos - length
    if(text.toString() !== "")
      updateAnnotationInput(
        {
          ...annotationInput,
          open: true,
          start,
          end: start + length,
          boxX: e.nativeEvent.offsetX,
          boxY: e.nativeEvent.offsetY
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
  const annots = annotations.map(a => ({ offset: a.target[0].selector.start, length: a.target[0].selector.end - a.target[0].selector.start, tooltip: a.body[0].value.text }))
  const annotatedDescription =
    <div className="annotated-description">
      { annotationInput.open &&
        <div className="annotation-input" style={{ left: annotationInput.boxX, top: annotationInput.boxY + 70 }}>
          <CloseButton onClick={ (e) => { updateAnnotationInput({ ...annotationInput, open: false }); } }/>
          <Input
            type="text"
            value={ annotationInput.text }
            onChange={ (e) => updateAnnotationInput({...annotationInput, text: e.target.value}) }
          />
          <Button onClick={ (e) => createAnnotation(ANNOTATION_TXT_INPUT, token, culturalHeritage.id, annotationInput) }>Create</Button>
        </div>
      }
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
