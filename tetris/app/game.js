const Board = require("./board");

function Game() {
    const board = new Board(24, 10);

    this.getBoard = () => board;
    this.start = () => {
        board.addNewPiece();
        return this;
    };
}

module.exports = Game;
