function Game(board) {
    const fps = 200;
    let intervalId;

    this.getBoard = () => board;
    this.getFps = () => fps;
    this.start = () => {
        this.init();
        intervalId = setInterval(this.tick, fps);
    };
    this.init = () => board.addNewPiece();
    this.tick = () => board.canMoveDown() ? board.moveDown() : board.addNewPiece();
    this.stop = () => clearInterval(intervalId);
}

module.exports = Game;
