import React, { Component } from 'react';
import CulturalHeritage from '../CulturalHeritage/CulturalHeritage';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PhotoIcon from 'react-icons/lib/md/photo-album';
import './style.css';

const Search = ({ token, culturalHeritages, favoriteItem }) => (
  <Container>
    <Row>
      <Col xs="9">
        { culturalHeritages.length === 0
        ? (
          <div className="search-not-found-box">
            <span>
              We could not find anything with your query. Try to look at all cultural heritages maybe?
            </span>
            <br />
            <NavLink to="/cultural-heritages" className="atlas-button" >
              <PhotoIcon /> Visit Cultural Heritages
            </NavLink>
          </div>
        ) : (
          <ul className="cultural-heritages">
            { culturalHeritages && culturalHeritages
              .sort((c1, c2) => c1.id - c2.id)
              .map(c => (
                <li key={ c.id }>
                  <CulturalHeritage returnTo="/search" culturalHeritage={ c } showCommentSummary={ true } shouldTruncate ={ true } favorite={ () => favoriteItem(token, c) } />
                </li>
              )
            )}
          </ul>
        )}
      </Col>
    </Row>
  </Container>
)

export default Search;
