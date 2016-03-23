// TODO - see if we can import better
import keyMirror from '../../../node_modules/fbjs/lib/keyMirror';
import $ from '../../../node_modules/jquery';

const MoveConstants = $.extend(keyMirror({
  MOVE_CREATE: null,
  MOVE_DESTROY: null
}), {
  api: keyMirror({
    GET_ALL_MOVES: null
  })
});

export default MoveConstants;
