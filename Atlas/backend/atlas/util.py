from textblob import TextBlob


class hidden_tag_extractor:
    def extract_keywords(self, text, limit=0):
        '''

        Args:
            text: the input string
            limit: maximum number of keywords to be returned

        Returns: a list of ranked keywords in descending order.

        '''
        blob = TextBlob(text)
        return list(set(blob.noun_phrases))
