from rest_framework import status
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import AllowAny
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from .serializers import AccountSerializer
from .models import Account


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
@permission_classes((IsAuthenticated, ))
def user(request):
    username = request.user.get_username()
    user = get_object_or_404(Account,username = username)
    serializer = AccountSerializer(user)
    return Response(serializer.data, status=status.HTTP_200_OK)