import AppDispatcher from '../dispatchers/AppDispatcher';
import MoveConstants from '../constants/MoveConstants';
import MoveApi from '../api/MoveApi';

const MoveActions = {
    create(move) {
        AppDispatcher.dispatch({
            actionType: MoveConstants.MOVE_CREATE,
            data: move
        });
        // Call MoveApi.createMove(name);
    },

    getMoves() {
        MoveApi.getAllMoves();
    }
};
export default MoveActions;
