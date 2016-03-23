import AppDispatcher from '../dispatchers/AppDispatcher';
import BoardConstants from '../constants/BoardConstants';
import BoardApi from '../api/BoardApi';

const BoardActions = {
    create(name) {
        AppDispatcher.dispatch({
            actionType: BoardConstants.BOARD_CREATE,
            name: name
        });
        // Call BoardApi.createBoard(name);
    },

    getBoard() {
        BoardApi.getBoard();
    }
};
export default BoardActions;
