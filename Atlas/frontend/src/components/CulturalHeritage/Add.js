import React from 'react';
import './style.css';
import { Form, FormGroup, Input, Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PlusIcon from 'react-icons/lib/fa/plus-circle';
import LeftIcon from 'react-icons/lib/fa/angle-left';
import { Errors } from '../../utils';
import Dropzone from 'react-dropzone';
import { WithContext as ReactTags } from 'react-tag-input';

const Page = ({ user, token, addCHInputs, addCHErrors, handleCHInputChange, addCHTag, deleteCHTag, addCH, goBack, handleDrop, imageUrl }) => (
  <Container>
    <NavLink className="atlas-button" onClick={goBack} to="/cultural-heritages"><LeftIcon /> Back</NavLink>
    <h1 style={{ textAlign: 'center' }}>Add Cultural Heritage</h1> 
    <hr />
    <Form>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Title:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="title"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Description:</label>
              </Col>
              <Col xs="9">
                <Input
                  name="description"
                  type="textarea"
                  rows="7"
                  onChange={handleCHInputChange}
                />
              </Col>
          </Row>
      </FormGroup>
      <FormGroup>
          <Row>
              <Col xs="3">
                  <label>Tags:</label>
              </Col>
              <Col xs="9">
                <ReactTags tags={ addCHInputs.tags || [] } handleAddition={ addCHTag } handleDelete={ deleteCHTag }/>
              </Col>
          </Row>
      </FormGroup>

      <FormGroup>
        <span className="atlas-button" onClick={ () => addCH(addCHInputs, token, imageUrl) }><PlusIcon /> Add Cultural Heritage</span>
      </FormGroup>
        <FormGroup>
            <Dropzone
                onDrop={handleDrop}
                multiple
                accept="image/*">

                <p>Drop your files or click here to upload</p>
            </Dropzone>
        </FormGroup>
      <Errors errors={ addCHErrors } />
    </Form>
  </Container>
)

export default Page;
