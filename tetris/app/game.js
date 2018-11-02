function Game(board, fps, gameRules) {
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
        let completedLines = board.getCompletedLines();
        board.updateBoard(completedLines);
        this.advance();
    };
    this.advance = () => board.canMoveActivePiece() ? board.moveActivePiece() : this.noAdvancePossible();
    this.noAdvancePossible = () => board.isBoardFull() ? this.gameOver() : this.keepPlaying();
    this.gameOver = () => {
        clearInterval(intervalId);
        isEnded = true;
    };
    this.keepPlaying = () => {
        board.blockActivePiece();
        board.addNewPiece();
    };
    this.rotateActivePiece = () => board.canRotatePiece() && board.rotateActivePiece();
    this.moveRight = () => board.canMoveRight() && board.moveRight();
    this.moveLeft = () => board.canMoveLeft() && board.moveLeft();
    this.isEnded = () => isEnded;
}

module.exports = Game;