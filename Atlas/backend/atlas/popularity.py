from datetime import datetime
from math import log
from textblob import TextBlob
from .popularity_constants import *
from .models import item_visit, comment, image_media_item


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
    item_visit_obj = item_visit.objects.filter(cultural_heritage_item=item)
    view_sec = item_visit_obj[0].duration() if item_visit_obj else 0
    num_comments = item.comment_set.count()
    num_favorites = item.favorited_amount
    admiration = COEFF_VIEW_SEC*view_sec + COEFF_NUM_COMMENTS*num_comments + COEFF_NUM_FAVORITES*num_favorites
    # completeness
    blob = TextBlob(item.description)
    description_complete = len(blob.noun_phrases) > COMPLETE_DESCRIPTION_NOUN_PHRASES
    tags_complete = item.tags.count() > COMPLETE_TAG_NUM
    imgs_complete = item.image_media_item_set.count()
    location_complete = item.longitude is not None
    completeness = description_complete + tags_complete + imgs_complete + location_complete

    popularity = log(1 + admiration) + COEFF_COMPLETENESS*completeness
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
    time_passed = (datetime.now() - creation_time).seconds/60
    freshness = max(0, INITIAL_FRESHNESS - time_passed)

    return COEFF_POPULARITY*popularity + COEFF_FRESHNESS*freshness
