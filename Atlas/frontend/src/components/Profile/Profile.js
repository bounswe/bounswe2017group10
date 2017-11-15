import React from 'react';
import { Card, CardBody,
  CardTitle, CardSubtitle,
  Container, Row, Col
} from 'reactstrap';
import { NavLink } from 'react-router-dom';
import PhotoIcon from 'react-icons/lib/md/photo-album';

const Profile = ({ user }) => ({
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
    </Container>
      )
  }
})

export default Profile;
