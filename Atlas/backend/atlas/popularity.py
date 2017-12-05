from datetime import datetime
from math import log

from textblob import TextBlob
from .models import item_visit
from .popularity_constants import *


def admiration_score(item):
    """
    Given a cultural heritage item this function returns its admiration score

    Args:
        item: A Cultural_Heritage object

    Returns: Admiration score of the Cultural_Heritage object.

    """
    item_visit_obj = item_visit.objects.filter(cultural_heritage_item=item)
    view_sec = item_visit_obj[0].duration if item_visit_obj else 0
    num_comments = item.comment_set.count()
    num_favorites = item.favorited_amount
    admiration_score = COEFF_VIEW_SEC * view_sec + COEFF_NUM_COMMENTS * num_comments + COEFF_NUM_FAVORITES * num_favorites
    return admiration_score


def completeness_score(item):
    """
    Fiven a cultural heritage item this function returns its completeness
    score.

    Args:
        item: A Cultural_Heritage object.

    Returns:Completeness score of the given Cultural_Heritage object.

    """
    blob = TextBlob(item.description)
    description_complete = len(blob.noun_phrases) >= COMPLETE_DESCRIPTION_NOUN_PHRASES
    tags_complete = item.tags.count() >= COMPLETE_TAG_NUM
    imgs_complete = item.image_media_item_set.count() >= COMPLETE_IMG_NUM
    location_complete = item.longitude is not None
    completeness_score = COEFF_DESCRIPTION_COMPLETE * description_complete + COEFF_TAGS_COMPLETE * tags_complete + \
                         COEFF_IMAGE_COMPLETE * imgs_complete + COEFF_LOCATION_COMPLETE * location_complete
    return completeness_score


def popularity_score(item):
    """
    Given a cultural heritage item, this function returns its
    popularity score.

    Popularity is calculated according to item popularity model as defined in
    item_popularity_model.ipynb

    Parameters
    ----------
    item: A Cultural_Heritage object.

    Returns
    -------
    out: Popularity score of the given Cultural_Heritage object.
    """
    # admiration
    admiration = admiration_score(item)
    # completeness
    completeness = completeness_score(item)

    popularity = COEFF_ADMIRATION * log(1 + admiration) + COEFF_COMPLETENESS * completeness
    return popularity


def trending_score(item):
    """
    Given a cultural heritage item, this function returns its
    trending score.

    Trending score is calculated according to item popularity model as defined in
    item_popularity_model.ipynb

    Parameters
    ----------
    item: A Cultural_Heritage object.

    Returns
    -------
    out: Trending score of the given Cultural_Heritage object.
    """
    popularity = popularity_score(item)
    # time passed
    creation_time = datetime.fromordinal(item.created_time.toordinal())
    time_passed = (datetime.now() - creation_time).total_seconds() / 60
    freshness = max(0, INITIAL_FRESHNESS - time_passed)

    return COEFF_POPULARITY * popularity + COEFF_FRESHNESS * freshness
