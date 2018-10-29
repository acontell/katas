function Game(board, fps) {
    const time = fps || 200;
    let isEnded;
    let intervalId;

    this.getBoard = () => board;
    this.getFps = () => time;
    this.start = () => {
        this.init();
        intervalId = setInterval(this.tick, time);
    };
    this.init = () => board.addNewPiece();
    this.tick = () => {
        board.clearLines();
        this.moveActivePiece();
    };
    this.moveActivePiece = () => board.canMoveActivePiece() ? board.moveActivePiece() : this.activePieceStuckSituation();
    this.activePieceStuckSituation = () => board.isBoardFull() ? this.gameOver() : board.addNewPiece();
    this.gameOver = () => {
        clearInterval(intervalId);
        isEnded = true;
    };
    this.isEnded = () => isEnded;
}

module.exports = Game;