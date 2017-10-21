import React from 'react';
import { Card, CardImg, CardText, CardBody,
  CardTitle, CardSubtitle, Button,
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
                  <CardTitle>
                    { user.firstname && user.lastname
                      ? user.firstname + " " + user.lastname + " (" + user.username + ")"
                      : user.username
                    }
                  </CardTitle>
                  <CardSubtitle>{ user.email }</CardSubtitle>
                  <hr />
                  <NavLink to="/cultural-heritages">
                    <Button><PhotoIcon /> Visit Cultural Heritages</Button>
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
