from django.shortcuts import get_object_or_404
from rest_framework import status, generics
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework.views import APIView
from .models import Account
from .serializers import AccountSerializer, AccountUpdateSerializer
from .permissions import KWArgCheck


class AuthRegister(APIView):
    """
    Register a new user.
    """
    serializer_class = AccountSerializer
    permission_classes = (AllowAny,)

    def post(self, request, format=None):
        serializer = self.serializer_class(data=request.data)
        if serializer.is_valid():
            serializer.save()
            return Response(serializer.data, status=status.HTTP_201_CREATED)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)


@api_view(['GET'])
@permission_classes((IsAuthenticated,))
def user(request):
    username = request.user.get_username()
    user = get_object_or_404(Account, username=username)
    serializer = AccountSerializer(user)
    return Response(serializer.data, status=status.HTTP_200_OK)


class AccountUpdate(generics.UpdateAPIView):
    """
    Update user profile.
    """
    serializer_class = AccountUpdateSerializer
    permission_classes = (IsAuthenticated, KWArgCheck,)
    lookup_field = 'username'

    def update(self, request, *args, **kwargs):
        partial = kwargs.pop('partial', False)
        instance = self.get_object()
        serializer = self.get_serializer(instance, data=request.data, partial=partial)
        serializer.is_valid(raise_exception=True)

        self.perform_update(serializer)
        return Response(serializer.data)

    def get_queryset(self):
        return Account.objects.filter()
