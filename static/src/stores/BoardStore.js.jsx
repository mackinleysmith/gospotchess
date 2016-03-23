'use strict';

import AppDispatcher from '../dispatchers/AppDispatcher';
import BoardConstants from '../constants/BoardConstants';
import { EventEmitter } from 'events';

const CHANGE_EVENT = 'BOARD_CHANGE';

let _state = {
  boardData: {}
};

function create(data) {
  // var move = { from: data.from, to: data.to };
  // _state.boardData.push(move);
}

function persistBoard(response) {
  _state.boardData = response;
}

class BoardStoreClass extends EventEmitter {

  getState() {
    return _state;
  }

  getBoard() {
    return _state.boardData;
  }

  addChangeListener(callback) {
    this.on(CHANGE_EVENT, callback);
  }

  removeChangeListener(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }

}

const BoardStore = new BoardStoreClass();

AppDispatcher.register((payload) => {
  switch (payload.actionType) {
    case BoardConstants.BOARD_CREATE:
        create(payload.data);
        break;

    // API Actions
    case BoardConstants.api.GET_BOARD:
        persistBoard(payload.response);
        break;

  }
  BoardStore.emit(CHANGE_EVENT);
});

export default BoardStore;
