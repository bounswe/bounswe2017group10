import {Map, Marker, GoogleApiWrapper} from 'google-maps-react';
import React from 'react';

export class MapContainer extends React.Component {

    componentDidMount() {

            if (navigator && navigator.geolocation) {
                navigator.geolocation.getCurrentPosition((pos) => {
                    this.props.getUser({ltd:pos.coords.latitude.toFixed(5),lng:pos.coords.longitude.toFixed(5)});
                })
            }


    }

    render() {
        return (

            <Map onClick={this.props.mapClick} center={{
                lat: (typeof this.props.lat !== 'undefined') ? this.props.lat:48,
                lng: (typeof this.props.lng !== 'undefined') ? this.props.lng:2
            }} google={this.props.google} zoom={8}
            style={{height:'800px', width: '1110px'}}>

                <Marker name={this.props.locationName} position={{"lat": this.props.lat, "lng": this.props.lng}} />

            </Map>
        );
    }
}

export default GoogleApiWrapper({
    apiKey: 'AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
})(MapContainer);
