import React from 'react';
import ChessBoard from './ChessBoard';
import MoveForm from './MoveForm';
import MoveList from './MoveList';
import MoveStore from '../stores/MoveStore';
import MoveActions from '../actions/MoveActions';

function getMoveListState() {
  return MoveStore.getMoves();
}

export default class ChessGame extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      // moveData: getMoveListState(),
      moveForm: {from: '', to: ''}
    };
    this.updateGameState = this._updateGameState.bind(this);
    this.setMoveFrom = this._setMoveFrom.bind(this);
    this.setMoveTo = this._setMoveTo.bind(this);
    this.handleMoveSubmit = this._handleMoveSubmit.bind(this);
    this.onChange = this._onChange.bind(this);
  }

  _updateGameState(new_state) {
    this.setState($.extend(true, {}, this.state, new_state));
  }
  _setMoveFrom(from) {
    this.updateGameState({moveForm: {from: from}});
  }
  _setMoveTo(to) {
    this.updateGameState({moveForm: {to: to}});
    setTimeout(function(){ $('.moveButton').click(); }, 1);
  }
  _handleMoveSubmit(move, on_success, on_error) {
    console.log("No longer needed...");
    // var moves = this.state.moveData;
    // console.log("Moves:", moves);
    // $.ajax({
    //   url: this.props.url,
    //   dataType: 'json',
    //   type: 'POST',
    //   data: move,
    //   success: function(data) {
    //     if (data.status == 'ok') {
    //       var move_keys = Object.keys(moves);
    //       if (move_keys.length > 0) {
    //         var max_move_num = Math.max(...move_keys.map((n) => parseInt(n) ));
    //         moves[(max_move_num + 1).toString()] = data.move;
    //       } else {
    //         moves["1"] = data.move;
    //       }
    //       this.updateGameState({moveData: moves});
    //       on_success();
    //     } else if (data.error != null) {
    //       on_error(data.error);
    //     } else {
    //       on_error("Something went horribly wrong!");
    //     }
    //   }.bind(this),
    //   error: function(xhr, status, err) {
    //     console.error(this.props.url, status, err.toString());
    //   }.bind(this)
    // });
  }

  componentDidMount() {
    MoveStore.addChangeListener(this.onChange);
    // this._updateMoveListIfNeeded();
  }

  componentWillUnmount() {
    MoveStore.removeChangeListener(this.onChange);
  }

  render() {
    return (
      <div className="chessGame">
        <h1>GoSpotChess</h1>
        <ChessBoard updateGameStateFn={this.updateGameState}
                    setFromFn={this.setMoveFrom}
                    setToFn={this.setMoveTo} />
        <hr />
        <MoveForm formState={this.state.moveForm}
                  updateGameStateFn={this.updateGameState}
                  onMoveSubmit={this.handleMoveSubmit} />
        <MoveList />
      </div>
    );
  }
  
  _onChange() {
    this.updateGameState({moveData: getMoveListState()});
  }

  _updateMoveListIfNeeded() {
    MoveActions.getMoves();
  }
}
