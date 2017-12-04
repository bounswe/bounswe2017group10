import { authPost, authDelete } from '../../utils';
import { API_URL } from '../../constants';
import { updateCulturalHeritage } from '../../actions/culturalHeritage';

export const favItem = (dispatch, token, culturalHeritage) => {
  const request = culturalHeritage.is_favorite ? authDelete : authPost;
  const favAmountChange = culturalHeritage.is_favorite ? (-1) : (+1);

  request(token, {
    url: API_URL + '/user/cultural_heritage_item/' + culturalHeritage.id + '/favorite'
  }).then(resp => {
    var newCH = Object.assign({}, culturalHeritage);
    newCH.favorited_amount += favAmountChange;
    newCH.is_favorite = !culturalHeritage.is_favorite;
    dispatch(updateCulturalHeritage(culturalHeritage.id, newCH));
  }).catch(err => {
    console.log("Error when favoriting an item");
    console.log(err);
  });
}
