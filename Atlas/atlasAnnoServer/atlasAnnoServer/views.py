from rest_framework import generics, mixins
from rest_framework import status
from rest_framework.response import Response
from .models import annotation, target, body
from .serializers import  annotation_serializer, target_serializer, body_serializer

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
        body = None
        if 'body' in request.data:
            body = request.data['body']

        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)

        if (body is not None):
            print("naber")
            print(body)
            id_body = annotation.objects.filter(IRI=serializer.data['id'])
            body['annotation'] = id_body
            body_serial = body_serializer(data=body)
            body_serial.is_valid(raise_exception=True)
            body_serial.save()

        if len(targets) > 0:
            id = annotation.objects.filter(IRI=serializer.data['id'])
            for targ in targets:
                targ['annotation'] = id
                targ_serializer = target_serializer(data=targ)
                targ_serializer.is_valid(raise_exception=True)
                targ_serializer.save()


        headers = self.get_success_headers(serializer.data)

        return Response(serializer.data, status=status.HTTP_201_CREATED, headers=headers)

class annotationRetrieve(generics.ListAPIView):
    serializer_class = annotation_serializer
    lookup_field = target.IRI

    def get_queryset(self):
        query = self.kwargs.get('query')
        print(query)
        annots = annotation.objects.filter(target__IRI=query)
        print(len(annots))
        return annots

