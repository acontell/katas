function GameActions(loopFnc, clearFnc, interval) {
    let id;
    let isEnded = false;

    this.startLoop = tickFnc => id = loopFnc(tickFnc, interval);
    this.stopLoop = () => {
        clearFnc(id);
        isEnded = true;
    };
    this.isLoopEnded = () => isEnded;
}

module.exports = GameActions;