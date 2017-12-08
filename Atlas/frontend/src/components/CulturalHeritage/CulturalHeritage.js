import React from 'react';
import { Container, Row, Col } from 'reactstrap';
import { truncate } from '../../utils';
import HomeIcon from 'react-icons/lib/fa/home';
import BankIcon from 'react-icons/lib/fa/bank';
import TagIcon from 'react-icons/lib/fa/tag';
import Comment from './Comment';
import AtlasHeader from '../utils/AtlasHeader'
import CHFav from './CHFav';
import { NavLink } from 'react-router-dom';

const CulturalHeritage = ({ withCloseButton=false ,returnTo, close, culturalHeritage, shouldTruncate = false, showCommentSummary = false, showComments = false, favorite, withLink = true, showFavorite = true }) => {
  const ch = (
    <Row className="whitebox">
      <Col xs="5">
        { culturalHeritage.images.length > 0 ? (
          <img className="ch-image" alt="Cultural Heritage" src={ culturalHeritage.images[0].url } />
          ) : (
          <span>No Image</span>
          )
        }
      </Col>
      <Col xs="7">
        <h2>{ culturalHeritage.title }</h2>
        <hr />
        <p>{ shouldTruncate
          ? truncate(culturalHeritage.description)
          : culturalHeritage.description
          }
        </p> 
        { (culturalHeritage.country || culturalHeritage.city || culturalHeritage.tags.length !== 0) && <hr /> }
        { culturalHeritage.country &&
          <label className="small-label">
            <BankIcon />
            <span> { culturalHeritage.country }</span>
          </label>
        }
        { culturalHeritage.city &&
          <label className="small-label">
            <HomeIcon />
            <span> { culturalHeritage.city }</span>
          </label>
        }
        { culturalHeritage.tags.map(tag => (
          <label className="small-label" key={ tag['id'] }>
            <TagIcon />
            <span>{ tag['name'] }</span>
          </label>
        ))}
      </Col>
    </Row>
  );
  return (
    <Container>
        { withCloseButton &&
        <span className="atlas-button" style={{ marginTop: 10, marginBottom: 10 }} onClick={ close }>Close</span>}
      { withLink
      ? (
      <NavLink style={{ width: '100%' }} to={ "/cultural-heritages/" + culturalHeritage.id + "?returnTo=" + returnTo } className="cultural-heritage">
        { ch }
      </NavLink>


      ) : <div>{ ch }</div>}
      <div style={{ width: '100%', float: 'right', fontSize: 13, textAlign: 'right' }}>
        { showFavorite && <CHFav culturalHeritage={ culturalHeritage } favorite={ favorite } /> }
        { showCommentSummary && (
            <span>{ culturalHeritage.comments.length } Comments</span>
        )}

      </div>
      {showComments && (culturalHeritage.comments.length !== 0) &&
        (<AtlasHeader text="Comments"/>)
      }
      { showComments && culturalHeritage.comments.map(comment => (
      <Row key={ Math.random() } className="whitebox" style={ { marginTop: 20 } }>
        <Col xs="12">
            <Comment comment={ comment } />
        </Col>
      </Row>
      ))}
    </Container>
  );
}

export default CulturalHeritage;
