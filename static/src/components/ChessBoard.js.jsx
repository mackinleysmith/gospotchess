import React from 'react';
import BoardSquare from './BoardSquare';
import BoardStore from '../stores/BoardStore';
import MoveStore from '../stores/MoveStore';
import BoardActions from '../actions/BoardActions';
import styles from '../css/board.scss';

var colLetters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
var rowNumbers = [ 8,   7,   6,   5,   4,   3,   2,   1 ];

function getBoardState() {
  return BoardStore.getBoard();
}

export default class ChessBoard extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      data: getBoardState(),
      activated: '',
      possibleMoves: []
    };
    this.onChange = this._onChange.bind(this);
    this.updateBoardIfNeeded = this._updateBoardIfNeeded.bind(this);
    this.setBoardState = this._setBoardState.bind(this);
    console.log("Board Data:", this.state.data);
  }

  componentDidMount() {
    BoardStore.addChangeListener(this.onChange);
    this.updateBoardIfNeeded();
  }

  componentWillUnmount() {
    BoardStore.removeChangeListener(this.onChange);
  }

  render() {
    console.log("Rendering board!");
    console.log(this.state.possibleMoves);
    var rows = rowNumbers.map(function(row_num) {
      var columns = colLetters.map(function(col_letter) {
        var position = col_letter.toString() + row_num;
        return (
            <BoardSquare position={position}
              occupant={this.state.data[position]}
              activated={this.state.activated == position}
              possibleMove={(this.state.possibleMoves.indexOf(position) > -1)}
              setFromFn={this.props.setFromFn}
              setToFn={this.props.setToFn}
              setBoardStateFn={this.setBoardState}
              colLetter={col_letter} />
        );
      }.bind(this));
      columns.push( <td className={styles.numCell}>{row_num}</td> );
      return ( <tr rowNum={row_num}>{columns}</tr> );
    }.bind(this));
    var letterCells = colLetters.map((letter) =>
      ( <td className={styles.numCell}>{letter}</td> ));
    rows.push( <tr>{letterCells}</tr> );

    return (
        <div className={styles.boardWrapper}>
          <table className={styles.chessBoard}>
            <tbody>{rows}</tbody>
          </table>
        </div>
    );
  }

  _onChange() {
    console.log("Board change!");
    this.setState({data: getBoardState(), activated: '', possibleMoves: []});
  }

  _updateBoardIfNeeded() {
    console.log("Updating board...");
    BoardActions.getBoard();
  }

  _setBoardState(new_state) {
    this.setState($.extend(true, {}, this.state, new_state));
  }
}

