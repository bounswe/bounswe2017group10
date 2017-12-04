import {Map, Marker, GoogleApiWrapper} from 'google-maps-react';
import React from 'react';

export class MapContainer extends React.Component {

    render() {
        return (
            <Map  onClick={this.props.mapClick} initialCenter={{
                lat: 48,
                lng: 2
            }} google={this.props.google} zoom={8}  >

                <Marker name={this.props.locationName} position={{"lat": this.props.lat, "lng": this.props.lng}} />

            </Map>
        );
    }
}

export default GoogleApiWrapper({
    apiKey: 'AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
})(MapContainer);