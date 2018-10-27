const Board = require('./board');

function Game() {
    const board = new Board(24, 10);
    const fps = 200;
    let intervalId;

    this.getBoard = () => board;
    this.init = () => board.addNewPiece();
    this.tick = () => board.moveDown();
    this.getFps = () => fps;
    this.start = () => {
        this.init();
        intervalId = setInterval(() => board.moveDown(), fps);
    };
    this.stop = () => clearInterval(intervalId);
}

module.exports = Game;
