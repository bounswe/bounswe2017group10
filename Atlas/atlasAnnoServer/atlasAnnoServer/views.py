from rest_framework import generics, mixins
from rest_framework import status
from rest_framework.response import Response
from .models import annotation, target, body
from .serializers import  annotation_serializer, target_serializer

class annotationView(generics.ListCreateAPIView):

    queryset = annotation.objects.get_queryset().order_by('id')
    serializer_class = annotation_serializer

    def perform_create(self, serializer):
        serializer.save()

    def get_queryset(self):
        return annotation.objects.get_queryset().order_by('id')

    def create(self, request, *args, **kwargs):

        targets=[]
        if 'targets' in request.data:
            targets = request.data['targets']

        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)

        if len(targets) > 0:
            id = serializer.data['id']
            for targ in targets:

                targ['annotation'] = id
                targ_serializer = target_serializer(data=targ)
                targ_serializer.is_valid(raise_exception=True)
                targ_serializer.save()

        headers = self.get_success_headers(serializer.data)

        return Response(serializer.data, status=status.HTTP_201_CREATED, headers=headers)
