import React from 'react';
import { Card, CardBody,
  CardTitle, CardSubtitle, Button,
  Container, Row, Col
} from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PhotoIcon from 'react-icons/lib/md/photo-album';
import CulturalHeritage from '../CulturalHeritage/CulturalHeritage';

const Profile = ({ user, userItems }) => ({
  render() {
    return (
      <Container>
        <Row>
          <Col xs="9">
            <div>
              <Card>
                <CardBody>
                  <CardTitle>Welcome
                    { user.firstname && user.lastname
                      ? " " + user.firstname + " " + user.lastname + " (" + user.username + ")"
                      : " " +user.username
                    }
                  </CardTitle>
                  <CardSubtitle>{ user.email }</CardSubtitle>
                  <hr />
                  <NavLink className="atlas-button" to="/cultural-heritages">
                    <PhotoIcon /> Visit Cultural Heritages
                  </NavLink>
                </CardBody>
              </Card>
            </div>
          </Col>
        </Row>
        <Row>
          <Col xs="9">
            <h1 className="my-items-title">My Cultural Heritage Items</h1>
            <ul className="cultural-heritages">
                {  userItems && userItems
                    .sort((c1, c2) => c1.id - c2.id)
                    .map(c => (
                            <NavLink key={ c.id } to={ "/cultural-heritages/" + c.id }>
                              <li className="cultural-heritage">
                                <CulturalHeritage culturalHeritage={ c } shouldTruncate ={ true }/>
                              </li>
                            </NavLink>
                        )
                    )}
            </ul>
          </Col>
        </Row>
    </Container>
      )
  }
})

export default Profile;
