// TODO - see if we can import better
import keyMirror from '../../../node_modules/fbjs/lib/keyMirror';
import $ from '../../../node_modules/jquery';

const BoardConstants = $.extend(keyMirror({
  BOARD_CREATE: null,
  BOARD_DESTROY: null
}), {
  api: keyMirror({
    GET_BOARD: null
  })
});

export default BoardConstants;
