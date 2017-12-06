import React from 'react';
import CulturalHeritage from './CulturalHeritage';
import './style.css';
import { Col, Row, Container } from 'reactstrap';
import { NavLink } from 'react-router-dom';
import AtlasHeader from '../utils/AtlasHeader';
import AtlasMap from '../Maps/NearbyMap';
import LeftIcon from 'react-icons/lib/fa/angle-left';

const Page = ({ user, token, location, loadUserLocation, nearbyItems, loadNearbyCulturalHeritages, closeHelp, loadMore, enableLoadMore, favoriteItem }) => (
    <Container>

        <Row classname="atlas-map">

            <Col xs="11">
                <AtlasHeader text="Nearby Items"/>
                <NavLink className="atlas-button" to="/profile">
                    <LeftIcon />
                    Back
                </NavLink>
                <AtlasMap token={ token } items= { nearbyItems } getUserLoc = {loadUserLocation} loadHeritages={ loadNearbyCulturalHeritages }center={ user.location }></AtlasMap>
            </Col>

        </Row>

    </Container>

)

export default Page;

