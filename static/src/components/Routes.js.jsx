import React from 'react';
import { Router, Route, IndexRoute, hashHistory } from 'react-router';
import ChessGame from './ChessGame';

export default class Routes extends React.Component {
  render() {
    return (
    <Router history={hashHistory}>
        <Route handler={ChessGame}>
          <Route path="/" component={ChessGame}>
            // <IndexRoute component={ItemManagement}/>

            // <Route path="/items/:itemId" component={ItemPage}/>

            // <Route path="/catalogs" component={CatalogManagement}/>
            // <Route path="/properties" component={PropertiesManagement}/>
            // <Route path="*" component={NoRouteMatch}/>
          </Route>
        </Route>
    </Router>);
  }
}
