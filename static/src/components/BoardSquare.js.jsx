import React from 'react';
import styles from '../css/board.scss';

export default class BoardSquare extends React.Component {
  constructor(props) {
    super(props);
    //this.state = {
    //  activated: false,
    //  possibleMove: false
    //};
    this.handleSquareClick = this._handleSquareClick.bind(this);
  }
  _handleSquareClick(e) {
    console.log(this.props.position, this.props.occupant);
    if ($('input.fromField').val().length == 0) {
      if (this.props.occupant == "") return;
      this.props.setFromFn(this.props.position);
      this.props.setBoardStateFn({activated: this.props.position, possibleMoves: []});
      $.get("/api/moves/available", {from: this.props.position}, function(data) {
        this.props.setBoardStateFn({
          possibleMoves: Object.keys(data.moves).map((k) => data.moves[k].to)
        });
      }.bind(this));
    } else {
      // if (this.props.occupant != "") return;
      this.props.setToFn(this.props.position);
      this.props.setBoardStateFn({activated: '', possibleMoves: []});
    }
  }
  render() {
    var occupantImage = '';
    if (this.props.occupant != null && this.props.occupant.length) {
      var image_path = "assets/pieces/" + this.props.occupant + ".png";
      occupantImage = ( <img src={image_path} className="piece-img" /> );
    }
    var cellClass = styles.boardSquare;
    if (this.props.activated === true) cellClass += ' ' + styles.activated;
    if (this.props.possibleMove === true) cellClass += ' ' + styles.possibleMove;
    // console.log("Rendering cell", this.props.position);
    return (
        <td ref="cell"
            className={cellClass}
            position={this.props.position}
            possibleMove={this.props.possibleMove}
            colLetter={this.props.col_letter}
            onClick={this.handleSquareClick}>
          {occupantImage}
        </td>
    );
  }
}
