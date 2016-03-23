import React from 'react';
import MoveActions from '../actions/MoveActions';

export default class MoveForm extends React.Component {
  constructor(props) {
    super(props);
    this.setFormState = this._setFormState.bind(this);
    this.handleFromChange = this._handleFromChange.bind(this);
    this.handleToChange = this._handleToChange.bind(this);
    this.handleSubmit = this._handleSubmit.bind(this);
  }
  _setFormState(new_form_state) {
    this.props.updateGameStateFn({moveForm: new_form_state});
  }
  _handleFromChange(e) {
    this.setFormState({from: e.target.value});
  }
  _handleToChange(e) {
    this.setFormState({to: e.target.value});
  }
  _handleSubmit(e) {
    e.preventDefault();
    $('.moveErrors').html('');
    var from = this.props.formState.from.trim();
    var to = this.props.formState.to.trim();
    if (!from || !to) return;
    MoveActions.create({from: from, to: to});
    // TODO: depend on some sort of callback or event for this.
    this.setFormState({from: '', to: ''});

    // this.props.onMoveSubmit({from: from, to: to}, function() {
    //   this.setFormState({from: '', to: ''});
    // }.bind(this), function(err) {
    //   $('.moveErrors').html(err);
    // }.bind(this));
  }
  render() {
    return (
      <form className="moveForm" onSubmit={this.handleSubmit}>
        <div className="moveErrors"></div>
        <input
          type="text"
          placeholder="From..."
          className="fromField"
          value={this.props.formState.from}
          onChange={this.handleFromChange} />
        <input
          type="text"
          placeholder="To..."
          className="toField"
          value={this.props.formState.to}
          onChange={this.handleToChange} />
        <input type="submit" value="Post" className="moveButton" />
      </form>
    );
  }
}
