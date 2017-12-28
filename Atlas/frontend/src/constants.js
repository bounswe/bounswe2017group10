// Modify these in development environment
export const SERVER_IP = 'http://174.129.53.155';
export const BACKEND_PORT = '81';
export const ANNOTATIONS_PORT = '82';

export const TRUNCATE_LENGTH = 200;
export const CULTURAL_HERITAGE_PAGINATION_LIMIT = 5;
export const COMMENT_PAGINATION_LIMIT = 10;
export const RECOMMENDATION_LIMIT = 5;
export const ANNOT_IMG_SELECTOR_KEY = 'imageSelector';
export const ANNOT_TXT_SELECTOR_KEY = 'textSelector';
export const ANNOTATION_TXT_INPUT = 1
export const ANNOTATION_IMG_INPUT = 2

// Don't touch!
export const API_URL = SERVER_IP + ':' + BACKEND_PORT;
export const ANNOTATION_SERVER_URL = SERVER_IP + ':' + ANNOTATIONS_PORT;
