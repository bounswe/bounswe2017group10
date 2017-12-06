import {Map, GoogleApiWrapper} from 'google-maps-react';
import React from 'react';
import CulturalHeritage from '../CulturalHeritage/CulturalHeritage';
import Marker from './AtlasMarker'

export class AtlasMap extends React.Component {

    componentDidMount() {
        this.props.mouseOffMarker();
        if (navigator && navigator.geolocation && this.props.center==null) {
            navigator.geolocation.getCurrentPosition((pos) => {
                this.props.getUserLoc({ltd:pos.coords.latitude.toFixed(5),lng:pos.coords.longitude.toFixed(5)});
                this.props.loadHeritages(this.props.token, this.props.center);
            })
        }
    }

    getCenter = (mapProps,map) =>{
        console.log(map.center.lat());
        this.props.getUserLoc({ltd:map.center.lat(),lng:map.center.lng()});
        this.props.mouseOffMarker();
    }


    render() {
        return (
            <Map onClick={this.props.mapClick} center={{
                lat: (this.props.center != null) ? this.props.center.ltd:48,
                lng: (this.props.center != null) ? this.props.center.lng:2
            }} google={this.props.google} zoom={8}
                 style={this.props.activeMarker==-1? {height:'900px', width: '1130px'}:{height:'400px', width: '1130px'}}
            onDragend={ this.getCenter }>

                {this.props.items && this.props.items
                    .map(c => (
                        <Marker id= { c.id } item = { c } onMouseover="handle()" mouseOnMarker={ this.props.mouseOnMarker } mouseOffMarker = { this.props.mouseOffMarker }
                                onMouseout="handle()" onClick="handle()" activeMarker= { this.props.activeMarker } isMouseOver={ false } title = { c.title } name={ this.props.locationName } position={{"lat": c.latitude, "lng": c.longitude}} />
                    )
                    )}
            </Map>
        );
    }
}

export default GoogleApiWrapper({
    apiKey: 'AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
})(AtlasMap);
