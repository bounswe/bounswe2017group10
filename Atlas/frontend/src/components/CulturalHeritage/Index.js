import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PlusIcon from 'react-icons/lib/fa/plus-circle';
import CloseIcon from 'react-icons/lib/fa/times-circle';

const Page = ({ user, token, culturalHeritages, helpOpen, paginationNextUrl, loadingMore, canLoadMore, loadCulturalHeritages, closeHelp, loadMore, enableLoadMore }) => (
  <Container>
    { helpOpen &&
      <div className="help-1">
        <span className="close-help" onClick={ closeHelp }><CloseIcon /></span>
        <div className="clearfix" />
        <p>Welcome to Atlas!</p>
        <p>
          Here, you can view different kinds of
          cultural heritages and see the details of each one by clicking to it.
        </p>
      </div>
    }
    <Row>
      <Col xs="9">
        <NavLink className="atlas-button" to="/cultural-heritages/new"><PlusIcon /> New Cultural Heritage</NavLink>
        <ul className="cultural-heritages">
          { culturalHeritages && culturalHeritages
            .sort((c1, c2) => c1.id - c2.id)
            .map(c => (
              <NavLink key={ c.id } to={ "/cultural-heritages/" + c.id }>
                <li>
                  <CulturalHeritage culturalHeritage={ c } showCommentSummary={ true } shouldTruncate ={ true }/>
                </li>
              </NavLink>
            )
          )}
        </ul>
        { canLoadMore && (
          <div style={{ width: "100%", textAlign: "center" }} className="atlas-button" onClick={ () => loadMore(token, paginationNextUrl) }>Load More</div>
        )}
      </Col>
    </Row>
  </Container>
)

export default Page;

