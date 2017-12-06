import {Map, Marker, GoogleApiWrapper} from 'google-maps-react';
import React from 'react';
import CulturalHeritage from '../CulturalHeritage/CulturalHeritage';

export class AtlasMap extends React.Component {

    componentDidMount() {

        if (navigator && navigator.geolocation) {
            navigator.geolocation.getCurrentPosition((pos) => {
                this.props.getUserLoc({ltd:pos.coords.latitude.toFixed(5),lng:pos.coords.longitude.toFixed(5)});
                this.props.loadHeritages(this.props.token, this.props.center);
            })
        }
    }

    render() {
        return (
            <Map onClick={this.props.mapClick} center={{
                lat: (this.props.center != null) ? this.props.center.ltd:48,
                lng: (this.props.center != null) ? this.props.center.lng:2
            }} google={this.props.google} zoom={8}
                 style={{height:'800px', width: '1110px'}}>

                {this.props.items && this.props.items
                    .map(c => (
                        <Marker title = { c.title } name={this.props.locationName} position={{"lat": c.latitude, "lng": c.longitude}} />
                    )
                    )}


            </Map>
        );
    }
}

export default GoogleApiWrapper({
    apiKey: 'AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
})(AtlasMap);
