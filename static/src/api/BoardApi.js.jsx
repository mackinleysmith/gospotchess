// REFERENCE: http://www.code-experience.com/async-requests-with-react-js-and-flux-revisited/
import AppDispatcher from '../dispatchers/AppDispatcher';
import BoardConstants from '../constants/BoardConstants';
import $ from 'jquery';

var API_URL = 'api';
var BOARD_SERVICE_URL = "http://localhost:9000/api/board";

var _pendingRequests = {};

function abortPendingRequests(key) {
    if (_pendingRequests[key]) {
        _pendingRequests[key]._callback = function(){};
        _pendingRequests[key].abort();
        _pendingRequests[key] = null;
    }
}

//function token() {
//    return UserStore.getState().token;
//}
// GSC_ITEMS_SERVICE_HOST
function makeUrl(part) {
    API_URL = BOARD_SERVICE_URL;
    return  API_URL + part;
}

function dispatch(key, response, params) {
    var payload = {actionType: key, response: response};
    if (params) {
        payload.queryParams = params;
    }
    //AppDispatcher.handleRequestAction(payload);
    AppDispatcher.dispatch(payload);
}

// return successful response, else return request Constants
function processResponse(key, params) {
    return function (response, textStatus, jqXHR) {
        //if (err && err.timeout === TIMEOUT) {
        //    dispatch(key, Constants.request.TIMEOUT, params);
        //} else if (response.status === 400) {
        //    UserActions.logout();
        //} else if (!response.ok) {
        //    dispatch(key, Constants.request.ERROR, params);
        //} else {
            dispatch(key, response, params);
        //}
    };
}

function get(url, params) {
  return $.ajax({
    method: 'GET',
    url: url,
    data: params,
    xhrFields: {withCredentials: true},
  });
}

// ****** TODO MOVE TO HELPER, probably just need when entry.js loads
// Set in user store?
function getCookie(name) {
    var value = "; " + document.cookie;
    var parts = value.split("; " + name + "=");
    if (parts.length == 2)
    {
        const part2 = parts.pop();
        return part2.split(";").shift();
    }
}

// TODO - refer to http://api.jquery.com/jquery.ajax/
// and change up the jqXHR response for this stuff, will make
// things easier to work with for processing
class BoardApiClass {
  getBoard() {
    var url = makeUrl("/list");
    var key = BoardConstants.api.GET_BOARD;
    var params = {};
    abortPendingRequests(key);
    //dispatch(key, Constants.request.PENDING, params);
    _pendingRequests[key] = get(url, params).done(
        processResponse(key, params)
    );
  }
}

const BoardApi = new BoardApiClass();
export default BoardApi;
