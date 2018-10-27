const Board = require("./board");

function Game() {
    const board = new Board(10, 24);

    this.getBoard = () => board;
    this.start = () => {
        board.addNewPiece();
    };
}

module.exports = Game;
