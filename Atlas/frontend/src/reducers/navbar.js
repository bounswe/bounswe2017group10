const initState = {
    dropdownOpen : false
};

const reducer = (state = initState, action) => {
    switch(action.type) {
        case 'OPEN_DROPDOWN':
            return {
                ...state,
                dropdownOpen : true
            };

        case 'CLOSE_DROPDOWN':
            return {
                ...state,
                dropdownOpen : false
            }

        default:
            return state;
    }
}

export default reducer;
