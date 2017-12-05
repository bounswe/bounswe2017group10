import React, { Component } from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { NavLink } from 'react-router-dom';
import LeftIcon from 'react-icons/lib/fa/angle-left';
import { Button, Form, FormGroup, Input, Col, Row, Container } from 'reactstrap';
import { authPut, getUrlParam } from '../../utils';
import { API_URL } from '../../constants';

class Show extends Component {
  constructor(props) {
    super(props);
    this.state = { startDate: new Date() };
  }

  componentWillMount() {
    console.log("mounting");
    this.setState({
      ...this.state,
      date: new Date()
    })
  }

  componentWillUnmount() {
    const secsPassed = Math.round((new Date() - this.state.date) / 1000);
    authPut(this.props.token, {
      url: API_URL + '/user/visit_time',
      data: {
        duration: secsPassed
      }
    });
  }

  render() {
    return (
      this.props.culturalHeritage !== undefined &&
        <Container>
          <NavLink className="atlas-button" to={ getUrlParam('returnTo') }>
            <LeftIcon />
            Back
          </NavLink>
          <Row>
            <Col xs="12">
              <CulturalHeritage culturalHeritage={ this.props.culturalHeritage } shouldTruncate={ false } showComments={ true } favorite={ () => this.favoriteItem(this.props.token, this.props.culturalHeritage).bind(this) } withLink={ false }/>
            </Col>
          </Row>
          <Row>
            <Col xs="12">
              <FormGroup style={{ marginTop: 30 }}>
                <Form>
                    <Input
                      name="username_or_email"
                      type="textarea"
                      onChange={this.props.commentInputChange.bind(this) }
                      value={ this.props.commentInput }
                      className="comment-input"
                      placeholder="Add a comment to this cultural heritage"
                    />
                </Form>
              </FormGroup>
            </Col>
          </Row>
          <Row>
              <Col xs="12">
                  <div className="atlas-button" onClick={ () => this.postComment(this.props.token, this.props.culturalHeritage.id, this.props.commentInput).bind(this) }>Post Comment</div>
              </Col>
          </Row>
        </Container>
    )
  }
}

export default Show;
