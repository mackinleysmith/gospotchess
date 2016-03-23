import React from 'react';
import ReactDOM from 'react-dom';
// import Routes from './components/Routes';
import ChessGame from './components/ChessGame';

ReactDOM.render(
    <ChessGame url="/api/moves" pollInterval={2000} />,
    document.getElementById('gsc-chess'));
