from .constants import *
from .models import tag_user_score, hidden_tag_user_score


def update_tag_user_score(item, user, sign):
    """
    Given a cultural heritage item and user, this function updates its tag score.

    Parameters
    ----------
    item: A Cultural_Heritage object.
    user: An Account object
    sign: 1 or -1

    """
    for tag in item.tags.all():
        instance, created = tag_user_score.objects.get_or_create(user=user.pk, tag=tag)
        if created:
            instance.score = FAVORITE_TAG_SCORE * sign
        else:
            instance.score += FAVORITE_TAG_SCORE * sign
        instance.save()


def update_hidden_tag_user_score(item, user, sign):
    """
    Given a cultural heritage item and user, this function updates its hidden tag score.

    Parameters
    ----------
    item: A Cultural_Heritage object.
    user: An Account object
    sign: 1 or -1

    """
    for hidden_tag in item.hidden_tags.all():
        instance, created = hidden_tag_user_score.objects.get_or_create(user=user.pk, hidden_tag=hidden_tag)
        if created:
            instance.score = FAVORITE_HIDDEN_TAG_SCORE * sign
        else:
            instance.score += FAVORITE_HIDDEN_TAG_SCORE * sign
        instance.save()


def item_score_for_user(item,user):
    """
    Given a cultural heritage item and user, this function returns the items score for the user.

    Parameters
    ----------
    item: A Cultural_Heritage object.
    user: An Account object

    Returns
    -------
    score: score of the item for the given user.

    """
    score = 0
    for tag in item.tags.all():
        if tag_user_score.objects.filter(tag=tag,
                                         user=user.id).count() > 0:
            score += tag_user_score.objects.get(tag=tag, user=user.id).score
    for hidden_tag in item.hidden_tags.all():
        if hidden_tag_user_score.objects.filter(hidden_tag=hidden_tag,
                                                user=user.id).count() > 0:
            score += hidden_tag_user_score.objects.get(hidden_tag=hidden_tag,
                                                       user=user.id).score

    return score
