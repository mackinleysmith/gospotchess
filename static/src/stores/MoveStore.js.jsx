'use strict';

import AppDispatcher from '../dispatchers/AppDispatcher';
import MoveConstants from '../constants/MoveConstants';
import MoveApi from '../api/MoveApi';
import { EventEmitter } from 'events';

const CHANGE_EVENT = 'CHANGE';

let _state = { moves: [] };

function getNextMoveNum() {
  var move_keys = Object.keys(_state.moves);
  if (!move_keys.length) return "1";
  var max_move_num = Math.max(...move_keys.map((n) => parseInt(n) ));
  return (max_move_num + 1).toString();
}

function create(move) {
  MoveApi.postMove(move);
  // TODO: use callbacks to only make this happen on success?
  _state.moves[getNextMoveNum()] = move;
  MoveStore.emit(CHANGE_EVENT);
}

function persistMoves(response) {
  if (_state.moves == response.moves) return;
  _state.moves = response.moves;
  MoveStore.emit(CHANGE_EVENT);
}

class MoveStoreClass extends EventEmitter {

  getState() {
    return _state;
  }

  getMoves() {
    return _state.moves;
  }

  addChangeListener(callback) {
    this.on(CHANGE_EVENT, callback);
  }

  removeChangeListener(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }

}

const MoveStore = new MoveStoreClass();

AppDispatcher.register((payload) => {
  switch (payload.actionType) {
    case MoveConstants.MOVE_CREATE:
        create(payload.data);
        break;

    // API Actions
    case MoveConstants.api.GET_ALL_MOVES:
        persistMoves(payload.response);
        break;

  }
});

export default MoveStore;
