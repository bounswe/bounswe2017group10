import React from 'react';
import {Row} from 'reactstrap';
import './style.css'

const AtlasHeader = ({ text }) => (
    <Row className="atlasheader">
       <h1>{text}</h1>
    </Row>
)

export default AtlasHeader;