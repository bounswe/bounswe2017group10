import React from 'react';
import { Card, CardBody,
  CardTitle, CardSubtitle,
  Container, Row, Col
} from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PhotoIcon from 'react-icons/lib/md/photo-album';
import unknown from '../../assets/images/unknown.png'
import atlasImg from '../../assets/images/atlas.jpeg';

const Profile = ({ user }) => ({
  render() {
    return (
      <Container>
        <img src={atlasImg} className="background-image" alt="background" />
        <Row className="whitebox-profile">
              <Col xs="3">

                      { user.profile_picture == null ? (
                          <img alt="Profile Picture" src={ unknown} />
                      ) : (
                          <img alt="Profile Picture" src={ user.profile_picture} />
                      )
                      }
                  </Col>
                  <Col xs="9">
                    <h2>
                  Welcome
                    { user.firstname && user.lastname
                      ? " " + user.firstname + " " + user.lastname + " (" + user.username + ")"
                      : " " +user.username
                    }
                    </h2>
                  { user.email }
                  <hr />
                  <NavLink to="/cultural-heritages" className="atlas-button" >
                    <PhotoIcon /> Visit Cultural Heritages
                  </NavLink>
                  </Col>
        </Row>
    </Container>
      )
  }
})

export default Profile;
