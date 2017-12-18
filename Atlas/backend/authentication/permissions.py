from rest_framework.permissions import BasePermission

class KWArgCheck(BasePermission):

    """
        Check if username given in url arguments matches authenticated user's username
    """
    def has_permission(self, request, view):
        return request.user.get_username() == view.kwargs['username']