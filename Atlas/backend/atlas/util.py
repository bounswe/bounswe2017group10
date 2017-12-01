from rake_nltk import Rake

class hidden_tag_extractor:
    r = Rake()

    def extract_keywords(self,text,threshold=-1,limit=0):
        '''

        Args:
            text: the input string
            threshold: threshold score for choosing keywords
            limit: maximum number of keywords to be returned

        Returns: a list of ranked keywords in descending order.

        '''
        self.r.extract_keywords_from_text(text)
        keywords =self.r.get_ranked_phrases_with_scores()
        if threshold >0:
            for i in range(0,len(keywords)):
             score,_ = keywords[i]
             if score <threshold:
                 keywords = keywords[:i]
                 break
        if limit >0 and limit < len(keywords):
            keywords = keywords[:limit]
        return keywords