import { connect } from 'react-redux';
import Page from '../../components/CulturalHeritage/Add';
import {
    updateCHInput, addCHFetch, addCHSuccess, addCHFail, clearAddCHInputs, clearImage,clearAddChErrors, uploadImage} from '../../actions/culturalHeritage';
import axios from 'axios';
import {API_URL,CLOUDINARY_API_KEY,CLOUDINARY_API_URL,CLOUDINARY_PRESET}  from '../../constants';


const mapStateToProps = state => {
  return {
    imageUrl : state.culturalHeritage.ImageUrl,
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
    handleDrop: files =>{
      console.log('naber');
      var returndata;
        const uploaders = files.map(file => {
            // Initial FormData
            const formData = new FormData();
            formData.append("file", file);
            formData.append("upload_preset", CLOUDINARY_PRESET);
            formData.append("api_key", CLOUDINARY_API_KEY);
            formData.append("timestamp", (Date.now() / 1000) | 0);

            return axios.post(CLOUDINARY_API_URL, formData, {
                headers: { "X-Requested-With": "XMLHttpRequest" },
            }).then(response => {
                const data = response.data;

                const fileURL = data.secure_url;
                const image_url = 'http://res.cloudinary.com/dsfusawmf/image/upload/v'+ data.version + '/' + data.public_id + '.png';
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
    goBack: () =>{
      dispatch(clearAddChErrors());
      dispatch(clearImage());
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
        headers: { 'Authorization': 'JWT ' + token },
        data: {
          title: addCHInputs.title,
          description: addCHInputs.description
        }}).then(resp => {
          if(addCHInputs.img_url !== undefined && addCHInputs.img_url !== "") {
              axios({
                  method: 'post',
                  url: API_URL + "/cultural_heritage_item/" + resp.data.id + "/image",
                  headers: { 'Authorization': 'JWT ' + token },
                  data: {
                      images: [{ url: addCHInputs.img_url }]
                  }}
              ).then(_resp => {
                  addSuccess(dispatch);
              })
          } else if (imageUrl.length!==0){
              axios({
                  method: 'post',
                  url: API_URL + "/cultural_heritage_item/" + resp.data.id + "/image",
                  headers: { 'Authorization': 'JWT ' + token },
                  data: {
                      images: imageUrl
                  }}
              ).then(_resp => {
                  addSuccess(dispatch);
              })
          } else{
              addSuccess(dispatch);
          }
        }).catch(err => {
          dispatch(addCHFail(err.response.data));
        });
    }
  }
}

const PageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);

export default PageContainer;
