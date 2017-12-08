import {connect} from 'react-redux';
import Page from '../../components/CulturalHeritage/Add';
import {
    updateCHInput,
    addCHFetch,
    addCHSuccess,
    addCHFail,
    clearAddCHInputs,
    clearAddChErrors,
    uploadImage,
    addCHTag,
    deleteCHTag,
    updateLocation
} from '../../actions/culturalHeritage';
import axios from 'axios';
import {API_URL} from '../../constants';


const mapStateToProps = state => {
    return {
        imageUrl: state.culturalHeritage.addCHInputs.img_url,
        user: state.auth.user,
        token: state.auth.token,
        addCHInputs: state.culturalHeritage.addCHInputs,
        addCHErrors: state.culturalHeritage.addCHErrors
    };
}

const addSuccess = (dispatch) => {
    dispatch(addCHSuccess());
    dispatch(clearAddCHInputs());
    window.location = '/cultural-heritages';
}

const mapDispatchToProps = dispatch => {
    return {
        handleDrop: files => {
            const uploaders = files.map(file => {
                // Initial FormData
                const formData = new FormData();
                formData.append("file", file);
                formData.append("upload_preset", "wak3gala");
                formData.append("api_key", "642824638492586");
                formData.append("timestamp", (Date.now() / 1000) | 0);

                // Make an AJAX upload request using Axios (replace Cloudinary URL below with your own)
                return axios.post("https://api.cloudinary.com/v1_1/dsfusawmf/image/upload", formData, {
                    headers: {"X-Requested-With": "XMLHttpRequest"},
                }).then(response => {
                    const data = response.data;
                    const image_url = 'http://res.cloudinary.com/dsfusawmf/image/upload/v' + data.version + '/' + data.public_id + '.png';
                    dispatch(uploadImage(image_url));
                    console.log(data);
                }).catch(err => {
                    console.log('Error while uploading: ' + err.data);
                    dispatch(uploadImage('Error'));
                });
            });

            // Once all the files are uploaded
            axios.all(uploaders).then(() => {

            });
        },
        goBack: () => {
            dispatch(clearAddChErrors());
        },
        handleCHInputChange: (event) => {
            const target = event.target;
            const name = target.name;
            const value = target.value;
            dispatch(updateCHInput(name, value));
        },
        addCH: (addCHInputs, token, imageUrl) => {
            dispatch(addCHFetch());
            axios({
                method: 'post',
                url: API_URL + '/cultural_heritage_item',
                headers: {'Authorization': 'JWT ' + token},
                data: {
                    title: addCHInputs.title,
                    description: addCHInputs.description,
                    tags: addCHInputs.tags.map(t => ( {name: t.text} )),
                    longitude: addCHInputs.lng,
                    latitude: addCHInputs.lat,
                    place_name: addCHInputs.locationName
                }
            }).then(resp => {
                if (addCHInputs.img_url !== undefined && addCHInputs.img_url !== "") {
                    axios({
                            method: 'post',
                            url: API_URL + "/cultural_heritage_item/" + resp.data.id + "/image",
                            headers: {'Authorization': 'JWT ' + token},
                            data: {
                                images: [{url: addCHInputs.img_url}]
                            }
                        }
                    ).then(_resp => {
                        addSuccess(dispatch);
                    })
                } else {
                    addSuccess(dispatch);
                }
            }).catch(err => {
                dispatch(addCHFail(err.response.data));
            });
        },
        addCHTag: (name) => dispatch(addCHTag(name)),
        updateGeo: (coords) => dispatch(updateLocation(coords)),
        deleteCHTag: (id) => dispatch(deleteCHTag(id)),
        searchLocation: (addCHInputs) => {

            //geocoding (obtain lat and lng from given address)
            axios({
                method: 'post',
                url: 'https://maps.googleapis.com/maps/api/geocode/json?address=' + addCHInputs.locationName + '&key=AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
            }).then(response => {


                if (response.data.status === "OK") {
                    const name1 = "lng";
                    const val1 = JSON.stringify(response.data.results[0].geometry.location.lng);
                    dispatch(updateCHInput(name1,Number(val1).toFixed(5)));
                    const name2 = "lat";
                    const val2 = JSON.stringify(response.data.results[0].geometry.location.lat);
                    dispatch(updateCHInput(name2,Number(val2).toFixed(5)));


                    const locationName = JSON.stringify(response.data.results[0].address_components[0].long_name);
                    dispatch(updateCHInput("locationName",locationName));

                    console.log(locationName);
                    console.log(Number(val1).toFixed(5)+" and "+Number(val2).toFixed(5));
                    console.log(addCHInputs.lat + " and " + addCHInputs.lng + addCHInputs.locationName);
                } else {
		
		    console.log("wrong location"+addCHInputs.locationName);
                   
                }


            })
        },
        mapClick: (mapProps, map, clickEvent) => {
            dispatch(updateCHInput("lat", clickEvent.latLng.lat().toFixed(5)));
            dispatch(updateCHInput("lng", clickEvent.latLng.lng().toFixed(5)));

            //reverse-geocoding (obtain address from lat and lng)
            axios({
                method: 'post',
                url: 'https://maps.googleapis.com/maps/api/geocode/json?latlng=' + clickEvent.latLng.lat().toFixed(5) + ',' + clickEvent.latLng.lng().toFixed(5) + '&key=AIzaSyCJ-k7tMf86LbMbQ-YAwi6-YAGmTx1z064'
            }).then(response => {


                if (response.data.status === "OK") {

                    const locationName = JSON.stringify(response.data.results[0].address_components[1].long_name);
                    dispatch(updateCHInput("locationName",locationName));

                    console.log(locationName);

                } else {

                    console.log("Could not find a location within map.");
                }


            })

        }

    }
}

    const PageContainer = connect(
        mapStateToProps,
        mapDispatchToProps
    )(Page);

    export default PageContainer;
