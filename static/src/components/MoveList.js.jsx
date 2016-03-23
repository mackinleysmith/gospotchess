import React from 'react';
import MoveStore from '../stores/MoveStore';
import MoveActions from '../actions/MoveActions';

var Move = React.createClass({
  render: function() {
    return (
      <li className="move">
        <span className="moveNumber">
          {this.props.moveNumber}.&nbsp;
        </span>
        {this.props.children}
      </li>
    );
  }
});

export default class MoveList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.onChange = this._onChange.bind(this);
  }

  componentDidMount() {
    MoveStore.addChangeListener(this.onChange);
    this._updateMoveListIfNeeded();
  }

  componentWillUnmount() {
    MoveStore.removeChangeListener(this.onChange);
  }

  render() {
    var moveNodes = Object.keys(this.state).reverse().map(function(move_num){
      var move = this.state[move_num]
      return (
        <Move moveNumber={move_num}>
          {move.from} {move.to}
        </Move>
      );
    }.bind(this));
    return (
      <div className="moveList">
        <ul>{moveNodes}</ul>
      </div>
    );
  }

  _onChange() {
    this.setState(MoveStore.getMoves());
  }

  _updateMoveListIfNeeded() {
    MoveActions.getMoves();
  }
}
