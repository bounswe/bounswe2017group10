import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Modal, Button, Form, FormGroup, Input, Col, Row, Container, Badge, Alert } from 'reactstrap';

const Page = ({ user, token, culturalHeritages, addCHInputs, isModalOpen, loadCulturalHeritages, handleCHInputChange, createCH, toggleAddCHModal }) => ({
  componentDidMount() {
    loadCulturalHeritages(token);
  },

  render() {
    return (
      <Container>
        <Modal
          isOpen={ isModalOpen }
          toggle={ toggleAddCHModal }
        >
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
                        onChange={handleCHInputChange}
                      />
                    </Col>
                </Row>
            </FormGroup>
            <FormGroup>
              <Button onClick={ () => createCH(addCHInputs, token) }>Add Cultural Heritage</Button>
            </FormGroup>
          </Form>
          <Button color="secondary" onClick={toggleAddCHModal}>Cancel</Button>
        </Modal>
        <Row>
          <Col xs="9">
            <span onClick={toggleAddCHModal}>Add Cultural Heritage</span>
            <ul className="cultural-heritages">
              { culturalHeritages.map(c => (
                <li key={ c.id } className="cultural-heritage-listitem">
                  <CulturalHeritage culturalHeritage={ c }/>
                </li>
              ))} 
            </ul>
          </Col>
        </Row>
      </Container>
      )
  }
})

export default Page;

