import React from 'react';
import AtlasHeader from '../utils/AtlasHeader';
import { Container, Row } from 'reactstrap';
import  atlas404  from '../../assets/images/atlas404.png';
import './style.css';

const NotFound = () => (
  <Container>
    <AtlasHeader text="Atlas Couldn't Find Specified Content"/>

    <Row className="atlas404">
      <img alt="404" src={ atlas404 } />
    </Row>

    <AtlasHeader text="Try Checking His Back"/>


  </Container>

)

export default NotFound;


