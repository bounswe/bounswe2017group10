import React from 'react';
import PropTypes from 'prop-types';
import CulturalHeritage from '../CulturalHeritage/CulturalHeritage';
import { Row } from 'reactstrap';
import './style.css';
import { InfoWindow } from 'react-google-maps';

const camelize = function(str) {
    return str.split(' ').map(function(word) {
        return word.charAt(0).toUpperCase() + word.slice(1);
    }).join('');
};

const evtNames = [
    'click',
    'dblclick',
    'dragend',
    'mousedown',
    'mouseout',
    'mouseover',
    'mouseup',
    'recenter',
];

const wrappedPromise = function() {
    var wrappedPromise = {},
        promise = new Promise(function (resolve, reject) {
            wrappedPromise.resolve = resolve;
            wrappedPromise.reject = reject;
        });
    wrappedPromise.then = promise.then.bind(promise);
    wrappedPromise.catch = promise.catch.bind(promise);
    wrappedPromise.promise = promise;

    return wrappedPromise;
}

export class Marker extends React.Component {

    componentDidMount() {
        this.markerPromise = wrappedPromise();
        this.renderMarker();
    }

    componentDidUpdate(prevProps) {
        if ((this.props.map !== prevProps.map) ||
            (this.props.position !== prevProps.position) ||
            (this.props.icon !== prevProps.icon)) {
            if (this.marker) {
                this.marker.setMap(null);
            }
            this.renderMarker();
        }
    }

    componentWillUnmount() {
        if (this.marker) {
            this.marker.setMap(null);
        }
    }

    renderMarker() {
        let {
            map, google, position, mapCenter, icon, label, draggable, title, item,
        } = this.props;
        if (!google) {
            return null
        }

        let pos = position || mapCenter;
        if (!(pos instanceof google.maps.LatLng)) {
            position = new google.maps.LatLng(pos.lat, pos.lng);
        }

        const pref = {
            map: map,
            position: position,
            icon: icon,
            label: label,
            title: title,
            draggable: draggable
        };
        this.marker = new google.maps.Marker(pref);

        evtNames.forEach(e => {
            this.marker.addListener(e, this.handleEvent(e));
        });

        this.markerPromise.resolve(this.marker);
    }

    getMarker() {
        return this.markerPromise;
    }

    handleEvent(evt) {
        return (e) => {
            const evtName = `on${camelize(evt)}`
            if (this.props[evtName]) {
                if(evtName=='onClick'){
                    this.props.mouseOnMarker(this.props.item.id);
                }else if(evtName=='onMouseover'){

                    console.log(this.props.item.id);
                }else if(evtName=='onMouseout'){
                    //this.props.mouseOffMarker();
                }
            }
        }
    }


    render() {
       if(this.props.activeMarker == this.props.item.id){ return(

        <Row style={{ marginTop:'450px' }}>
        <CulturalHeritage returnTo="/nearby-items"  withLink={ true } className="hovering-item" culturalHeritage={ this.props.item } showCommentSummary={ true } shouldTruncate ={ true }  />
        </Row>
            );}else{
           return null;
       }
    }
}

Marker.propTypes = {
    position: PropTypes.object,
    map: PropTypes.object
}

evtNames.forEach(e => Marker.propTypes[e] = PropTypes.func)

Marker.defaultProps = {
    name: 'Marker'
}

export default Marker