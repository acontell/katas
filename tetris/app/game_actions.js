let _ = require('lodash');

function GameActions(loopFnc, clearFnc) {
    this.startLoop = (tickFnc, interval) => loopFnc(tickFnc, interval);
    this.stopLoop = id => clearFnc(id);
}

module.exports = GameActions;