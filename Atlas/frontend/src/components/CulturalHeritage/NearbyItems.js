import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import AtlasHeader from '../utils/AtlasHeader';
import AtlasMap from '../Maps/NearbyMap';
import LeftIcon from 'react-icons/lib/fa/angle-left';

const Page = ({ user, token, mapCenter, dragMap, location, mouseOn, loadUserLocation, mouseOnMarker, mouseOffMarker, nearbyItems, loadNearbyCulturalHeritages, closeHelp, loadMore, enableLoadMore, favoriteItem }) => (
    <Container>

        <Row className="atlas-map">

            <Col xs="12">
                <AtlasHeader text="Nearby Items"/>
                <AtlasMap token={ token }  mapCenter={ mapCenter } dragMap={ dragMap } activeMarker= { mouseOn } mouseOnMarker={ mouseOnMarker } mouseOffMarker = { mouseOffMarker } items= { nearbyItems } getUserLoc = {loadUserLocation} loadHeritages={ loadNearbyCulturalHeritages }center={ user.location }></AtlasMap>
            </Col>

        </Row>

    </Container>

)

export default Page;

