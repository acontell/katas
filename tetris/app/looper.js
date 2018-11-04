function GameActions(loopFnc, clearFnc, interval) {
    let id;
    let isEnded = false;

    this.start = tickFnc => id = loopFnc(tickFnc, interval);
    this.stop = () => {
        clearFnc(id);
        isEnded = true;
    };
    this.isEnded = () => isEnded;
}

module.exports = GameActions;